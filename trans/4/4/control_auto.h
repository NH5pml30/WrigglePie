#pragma once

#include <map>
#include <set>
#include <vector>
#include <ranges>
#include <cassert>
#include <algorithm>
#include <sstream>
#include <string>
#include <fstream>
#include <string_view>

#include "config.h"

using namespace element_literals;

class control_automaton
{
public:
  grammar G;

private:
  struct LR0_core
  {
    unsigned rule_id;
    unsigned pos;

    friend std::strong_ordering operator<=>(LR0_core, LR0_core) = default;
  };

  //                     core      expects
  using state = std::map<LR0_core, std::set<grammar::terminal>>;

  // check if key sets are equal
  static bool state_eq(const state &lhs, const state &rhs)
  {
    if (lhs.size() != rhs.size())
      return false;
    for (auto it1 = lhs.begin(), it2 = rhs.begin(); it1 != lhs.end(); ++it1, ++it2)
      if (it1->first != it2->first)
        return false;
    return true;
  }

  using LR1_item = state::value_type;

  template<typename T>
  bool merge(T &lhs, T &rhs)
  {
    auto size = lhs.size();
    lhs.merge(rhs);
    return lhs.size() > size;
  }

  // merge values of equal key sets
  bool unite(state &lhs, state &rhs) {
    assert(lhs.size() == rhs.size());
    bool changes = false;
    for (auto it1 = lhs.begin(), it2 = rhs.begin(); it1 != lhs.end(); ++it1, ++it2)
    {
      assert(it1->first == it2->first);
      changes |= merge(it1->second, it2->second);
    }
    return changes;
  }

  // merge keys, merge values for equal keys
  bool merge_unite(state &lhs, state &rhs) {
    bool changes = false;
    for (auto it1 = lhs.begin(), it2 = rhs.begin(); it1 != lhs.end() && it2 != rhs.end(); ++it1)
    {
      while (it2 != rhs.end() && it2->first < it1->first)
      {
        auto saved = it2++;
        lhs.insert(rhs.extract(saved));
        changes = true;
      }
      if (it2 == rhs.end())
        break;

      if (it2->first == it1->first)
      {
        auto saved = it2++;
        changes |= merge(it1->second, saved->second);
        rhs.erase(saved);
      }
    }
    changes |= merge(lhs, rhs);
    return changes;
  }

  // compute epsilon-closure of a state
  state &closure(state &stt) const
  {
    std::vector<state::const_iterator> queue, queue_back;

    for (auto item = stt.begin(); item != stt.end(); ++item)
      queue.push_back(item);

    while (!queue.empty())
    {
      queue_back.clear();
      for (auto &&item : queue)
      {
        auto &&[rule_id, pos] = item->first;
        auto &rule_rhs = G[rule_id].second.rhs;

        if (pos < rule_rhs.size() && G.is_nonterminal(rule_rhs[pos]))
        {
          // next symbol is non-terminal, can go there by epsilon
          auto expects = G.first(rule_rhs | std::views::drop(pos + 1));
          if (expects.erase(grammar::eps) > 0)
            expects.insert(item->second.begin(), item->second.end());

          for (auto &&[nt, r_id] : G(rule_rhs[pos]))
          {
            auto [it, is_new] = stt.insert({{r_id, 0}, {}});
            it->second.insert(expects.begin(), expects.end());
            if (is_new)
              queue_back.push_back(it);
          }
        }
      }
      queue.swap(queue_back);
    }

    return stt;
  }

  struct shift_action
  {
    unsigned next_state;
  };

  struct reduce_action
  {
    unsigned rule_id;
  };

  //                          error/unused    shift         reduce
  using action = std::variant<std::monostate, shift_action, reduce_action>;

  std::vector<state> states_stg;
  std::vector<std::vector<action>> trans_table;

  template<class... Ts>
  unsigned emplace_state(Ts&&... args)
  {
    states_stg.emplace_back(std::forward<Ts>(args)...);
    trans_table.emplace_back();
    trans_table.back().resize(G.max_element);
    return (unsigned)(states_stg.size() - 1);
  }

  std::runtime_error reduce_reduce_conflict(unsigned rule1, unsigned rule2)
  {
    throw std::runtime_error(std::format("Reduce-reduce conflict: rule '{}' or '{}'",
                                         G[rule1].second.rule_ord + 1,
                                         G[rule2].second.rule_ord + 1));
  }

  std::runtime_error shift_reduce_conflict(std::string non_terminal, unsigned rule_id)
  {
    throw std::runtime_error(
        std::format("Shift-reduce conflict: shift non-terminal '{}' or reduce rule '{}'",
                    non_terminal, G[rule_id].second.rule_ord + 1));
  }

  void build_automaton(auto &&printer)
  {
    // start state: beginning of the 0th rule, expect end of file after that
    emplace_state(state{{{{0, 0}, {grammar::eof}}}});
    closure(states_stg[0]);

    std::set<unsigned> queue = {0}, queue_back;
    std::vector<bool> erased_indices;
    bool changes = true;
    while (!queue.empty() && changes)
    {
      changes = false;
      queue_back.clear();

      for (auto &&stt : queue)
      {
        // save current number of states to track the new ones
        unsigned n_states = (unsigned)states_stg.size();

        // CAUTION: `states_stg` may reallocate inside the loop,
        // brackets operator used for retrieving the updated state location
        for (auto iter = states_stg[stt].begin(); iter != states_stg[stt].end(); ++iter)
        {
          auto &&[core, expects] = *iter;
          if (core.pos < G[core.rule_id].second.rhs.size())
          {
            // inside the rule, add transitions based on the rule string
            auto c = G[core.rule_id].second.rhs[core.pos];

            // create a new state
            state next = {{{core.rule_id, core.pos + 1}, expects}};
            // it may seem as if we can move the closure to the end, but actually
            // it is needed for the correct build up or update of the `expects` set
            closure(next);

            int next_state = -1;

            // check old transition
            std::visit(overloaded{[](std::monostate) {},  // error/not set -- ok
                                  [&](shift_action act) {
                                    next_state = act.next_state;
                                  },  // shift -- update old (`expects` set or just building up)
                                  [&](reduce_action act) {
                                    throw shift_reduce_conflict(printer(c), act.rule_id);
                                  }},  // reduce -- conflict
                       trans_table[stt][c]);

            if (next_state == -1)
            {
              changes = true;
              // commit the new state
              // save current state from being copied inside the (possibly) reallocated vector, move
              // it instead
              auto buffer = std::move(states_stg[stt]);
              next_state = emplace_state();
              states_stg[stt] = std::move(buffer);
            }
            else if ((unsigned)next_state < n_states)
              // push old state for update
              queue_back.insert(next_state);

            // add the transition and add the data to the next state
            trans_table[stt][c] = shift_action{(unsigned)next_state};
            changes |= merge_unite(states_stg[next_state], next);
          }
          else
          {
            // the rule ended, add transitions based on the expects
            for (auto c : expects)
            {
              std::visit(overloaded{[](std::monostate) {},  // error/not set -- ok
                                    [&](shift_action act) {
                                      throw shift_reduce_conflict(printer(c), core.rule_id);
                                    },  // shift -- conflict
                                    [&](reduce_action act) {
                                      if (act.rule_id != core.rule_id)
                                        throw reduce_reduce_conflict(act.rule_id, core.rule_id);
                                    }},  // reduce -- cannot be the same rule, conflict
                         trans_table[stt][c]);
              trans_table[stt][c] = reduce_action{core.rule_id};
            }
          }
        }

        // LALR part: merge duplicate cores
        erased_indices.resize(states_stg.size() - n_states);
        std::fill(erased_indices.begin(), erased_indices.end(), false);
        unsigned cur_index = n_states;
        auto begins_new = states_stg.begin() + n_states;
        for (unsigned n_state = n_states; n_state < (unsigned)states_stg.size(); n_state++)
        {
          auto search_end = states_stg.begin() + n_state;
          unsigned upd_state;
          using namespace std::placeholders;
          if (auto o_iter = std::find_if(states_stg.begin(), search_end, std::bind(state_eq, std::ref(states_stg[n_state]), _1));
              o_iter != search_end)
          {
            // already found a state with the same LR0-core, reuse it
            // (it is not removed, because otherwise we would find the original equivalent)
            erased_indices[n_state - n_states] = true;

            upd_state = (int)(o_iter - states_stg.begin());
            unite(*o_iter, states_stg[n_state]);
            queue_back.insert(upd_state);
          }
          else
          {
            // state is unique, but it is moved, track it properly
            upd_state = cur_index++;
          }

          // repoint old references to the moved state
          for (grammar::element c = grammar::eof; c < G.max_element; c++)
            if (std::holds_alternative<shift_action>(trans_table[stt][c]) &&
                std::get<shift_action>(trans_table[stt][c]).next_state == n_state)
              trans_table[stt][c] = shift_action{upd_state};
        }

        // now remove the states
        auto updated_end = std::remove_if(begins_new, states_stg.end(), [&](auto &n_state_ref) -> bool {
          unsigned n_state = (unsigned)(&n_state_ref - states_stg.data());
          return erased_indices[n_state - n_states];
        });
        states_stg.resize(updated_end - states_stg.begin());
        for (unsigned i = n_states; i < (unsigned)states_stg.size(); i++)
          queue_back.insert(i);
      }

      queue.swap(queue_back);
    }
  }

  void print_LR1_item(auto &&printer, const LR1_item &item) const
  {
    auto &&rule = G[item.first.rule_id];
    auto rhs = rule.second.rhs;
    std::cout << '[' << printer(rule.first) << " ->";
    if (item.first.pos == 0)
      std::cout << '.';
    for (unsigned i = 0; i < rhs.size(); i++, std::cout << (i == item.first.pos ? "." : ""))
      std::cout << ' ' << printer(rhs[i]);
    std::cout << ",";
    for (auto el : item.second)
      std::cout << ' ' << printer(el);
    std::cout << ']';
  }

  void print_state(auto &&printer, const state &stt) const
  {
    for (auto &&item : stt)
      print_LR1_item(std::forward<decltype(printer)>(printer), item);
  }

  // helper type for the visitor #4
  template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
  // explicit deduction guide (not needed as of C++20)
  template<class... Ts>
  overloaded(Ts...) -> overloaded<Ts...>;

public:
  void print_table(auto &&printer) const
  {
    std::cout << "no;state";
    for (grammar::element i = grammar::eof; i < G.max_element; i++)
      std::cout << ';' << printer(i);
    std::cout << '\n';
    for (unsigned i = 0; i < states_stg.size(); i++)
    {
      std::cout << i << ';';
      print_state(std::forward<decltype(printer)>(printer), states_stg[i]);
      for (unsigned by = 0; by < trans_table[i].size(); by++)
      {
        std::cout << ';';
        std::visit(overloaded{[](std::monostate) { std::cout << 'x'; },
                              [&](shift_action act) {
                                if (!G.is_nonterminal(by))
                                  std::cout << "s,";
                                std::cout << act.next_state;
                              },
                              [](reduce_action act) { std::cout << 'r' << act.rule_id; }},
                   trans_table[i][by]);
      }
      std::cout << '\n';
    }
  }

  control_automaton(grammar G, auto &&printer) : G(std::move(G))
  {
    build_automaton(std::forward<decltype(printer)>(printer));
  }

  unsigned save_table(const std::string &filename) const
  {
    std::ofstream out(filename, std::ios::binary);
    struct written_data
    {
      uint32_t type : 2;
      uint32_t data : 30;
    };
    auto convert_variant_to_data = [](action act) -> written_data {
      written_data res{};
      std::visit(overloaded{[&](std::monostate) {},
                            [&](shift_action act) {
                              res.type = 1;
                              res.data = (uint32_t)act.next_state;
                            },
                            [&](reduce_action act) {
                              res.type = 2;
                              res.data = (uint32_t)act.rule_id;
                            }},
                 act);
      return res;
    };

    if (states_stg.size() >= (1 << 30))
      throw std::runtime_error("Too many states");
    if (G.size() >= (1 << 30))
      throw std::runtime_error("Too many rules");
    for (auto &row : trans_table)
      for (auto &cell : row)
      {
        auto data = convert_variant_to_data(cell);
        out.write((char *)&data, sizeof(data));
      }
    return (unsigned)states_stg.size();
  }
};
