#pragma once

#include <vector>
#include <variant>
#include <string>
#include <ranges>
#include <iostream>
#include <map>
#include <set>
#include <functional>

class grammar
{
public:
  using terminal = int;
  using nonterminal = int;
  using element = int;

  // int: ... -1 EOF non_terminal0 ... non_terminal_k terminal0 ... terminal_n=max_element ...
  int max_nonterminal, max_element;

  bool is_nonterminal(element d) const {
    return d > eps && d < max_nonterminal;
  }

  bool is_terminal(element d) const {
    return d >= max_nonterminal && d < max_element;
  }

  struct rule
  {
    std::vector<element> rhs;
    std::string atribute = "";
  };

  nonterminal start;
  std::vector<std::string> attr_types;

private:
  std::vector<std::pair<nonterminal, rule>> rules_stg;
  std::multimap<nonterminal, unsigned> rules;

  static std::multimap<nonterminal, unsigned> construct_rules(const std::vector<std::pair<nonterminal, rule>> &rules) {
    std::multimap<nonterminal, unsigned> res;
    for (unsigned i = 0; i < rules.size(); i++)
      res.insert({rules[i].first, i});
    return res;
  }

  mutable std::vector<std::set<terminal>> first_{max_nonterminal};

  void count_first() const {
    bool changed;
    do
    {
      changed = false;
      for (auto &&[a, alpha] : rules_stg)
      {
        auto saved = first_[a].size();
        first_[a].merge(first(alpha.rhs));
        changed = first_[a].size() > saved;
      }
    } while (changed);
  }

public:
  auto begin() const
  {
    return rules_stg.begin();
  }

  auto end() const
  {
    return rules_stg.end();
  }

  size_t size() const
  {
    return rules_stg.size();
  }

  static constexpr terminal eps = -1;
  static constexpr terminal eof = 0;

  grammar(std::vector<std::string> attr_types, int max_nonterminal, int max_element, std::vector<std::pair<nonterminal, rule>> rules)
      : max_nonterminal(max_nonterminal),
        max_element(max_element),
        start(rules[0].first),
        attr_types(std::move(attr_types)),
        rules_stg(std::move(rules)),
        rules(construct_rules(rules_stg))
  {
    count_first();
  }

  const std::pair<nonterminal, rule> &operator[](unsigned rule_id) const {
    return rules_stg[rule_id];
  }

  auto operator()(nonterminal nt) const {
    auto rng = rules.equal_range(nt);
    return std::ranges::subrange(rng.first, rng.second);
  }

  template<std::ranges::range RngT>
  std::set<terminal> first(RngT &&rng) const
  {
    std::set<terminal> res;
    for (auto it = std::begin(rng); ; ++it)
    {
      if (it == std::end(rng))
      {
        // empty
        res.insert(eps);
        break;
      }

      element begin = *it;
      if (is_terminal(begin))
      {
        // begins with a terminal
        res.insert(begin);
        break;
      }

      // begins with a non-terminal
      auto fa = first_[begin];
      bool was_eps = fa.erase(eps) > 0;
      res.merge(fa);
      if (!was_eps)
        break;
    }
    return res;
  }
};

namespace element_literals
{
  inline grammar::element operator"" _t(unsigned long long x) {
    return static_cast<grammar::terminal>(x);
  }

  inline grammar::element operator"" _t(char x) {
    return static_cast<grammar::terminal>(x);
  }

  inline grammar::element operator"" _nt(unsigned long long x) {
    return static_cast<grammar::nonterminal>(x);
  }

  inline grammar::element operator"" _nt(char x) {
    return static_cast<grammar::nonterminal>(x);
  }
}
