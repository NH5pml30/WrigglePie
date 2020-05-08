/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <array>
#include <map>
#include <set>
#include <stack>
#include <queue>

std::set<int> inverse_set(const std::set<int> &set, int n)
{
  std::set<int> res;
  auto it = set.cbegin();
  for (int v = 0; v < n; v++)
    if (it == set.cend() || v < *it)
      res.insert(v);
    else
      it++;
  return res;
}

class DFA
{
private:
  std::vector<std::map<char, int>> transitions;
  std::set<int> terminals;

public:
  DFA() { }

  friend std::istream & operator>>(std::istream &i, DFA &rhs);

  DFA & throw_out_unreachable()
  {
    std::set<int> reachable;
    std::stack<int> stack;
    stack.push(0);
    while (!stack.empty())
    {
      int stt = stack.top();
      stack.pop();
      if (reachable.count(stt))
        continue;
      reachable.insert(stt);
      for (auto next : transitions[stt])
        stack.push(next.second);
    }

    std::vector<std::map<char, int>> new_trans(reachable.size());
    std::set<int> new_terms;
    std::vector<int> fake2real(transitions.size());

    int i = 0;
    for (auto stt : reachable)
      fake2real[stt] = i++;

    i = 0;
    for (auto stt : reachable)
    {
      for (auto next : transitions[stt])
        new_trans[i][next.first] = fake2real[next.second];

      if (terminals.count(stt))
        new_terms.insert(i);
      i++;
    }

    terminals.swap(new_terms);
    transitions.swap(new_trans);
    return *this;
  }

  DFA & minimize()
  {
    throw_out_unreachable();

    int n = (int)transitions.size();
    transitions.push_back({});
    for (char ch = 'a'; ch <= 'z'; ch++)
      transitions.back()[ch] = n;
    for (int i = 0; i < n; i++)
      for (char ch = 'a'; ch <= 'z'; ch++)
        if (transitions[i].count(ch) == 0)
          transitions[i][ch] = n;
    n++;

    std::vector<std::map<char, std::set<int>>> back_transitions(n);

    for (int i = 0; i < n; i++)
      for (auto next : transitions[i])
        back_transitions[next.second][next.first].insert(i);

    std::queue<std::pair<int, int>> queue;
    std::vector<std::set<int>> distinguishable(n);

    for (auto u : terminals)
      for (auto v : inverse_set(terminals, n))
      {
        queue.push({u, v});
        queue.push({v, u});
        distinguishable[u].insert(v);
        distinguishable[v].insert(u);
      }

    while (!queue.empty())
    {
      auto [x, y] = queue.front();
      queue.pop();

      for (char ch = 'a'; ch <= 'z'; ch++)
        for (auto a : back_transitions[x][ch])
          for (auto b : back_transitions[y][ch])
            if (distinguishable[a].count(b) == 0)
            {
              distinguishable[a].insert(b);
              queue.push({a, b});
            }
    }
    n--;

    std::vector<int> state2equiv(n, -1);
    int new_states_size = 0;
    for (int i = 0; i < n; i++)
      if (state2equiv[i] == -1 && distinguishable[i].count(n))
      {
        state2equiv[i] = new_states_size;
        for (auto stt : inverse_set(distinguishable[i], n))
          state2equiv[stt] = new_states_size;
        new_states_size++;
      }

    std::vector<std::map<char, int>> new_trans(new_states_size);
    std::set<int> new_terms;

    for (int start = 0; start < n; start++)
      if (state2equiv[start] != -1)
        for (auto trans : transitions[start])
          if (trans.second != n && state2equiv[trans.second] != -1)
            new_trans[state2equiv[start]][trans.first] = state2equiv[trans.second];
    for (auto term : terminals)
      new_terms.insert(state2equiv[term]);

    transitions.swap(new_trans);
    terminals.swap(new_terms);

    return *this;
  }

  static bool are_isomorphic(const DFA &lhs, const DFA &rhs)
  {
    if (lhs.transitions.size() != rhs.transitions.size() ||
        lhs.terminals.size() != lhs.terminals.size())
      return false;

    std::map<int, int> state_bijection;
    std::vector<bool> found_preimage(rhs.transitions.size()), is_handled(lhs.transitions.size());

    std::stack<std::pair<int, int>> stack;
    stack.push({0, 0});
    while (!stack.empty())
    {
      auto [lhs_state, rhs_state] = stack.top();
      stack.pop();
      if (auto it = state_bijection.find(lhs_state); it != state_bijection.cend())
      {
        if (!found_preimage[rhs_state] || it->second != rhs_state)
          return false;
        continue;
      }
      else if (found_preimage[rhs_state])
        return false;
      else
      {
        if (lhs.terminals.count(lhs_state) != rhs.terminals.count(rhs_state))
          return false;
        found_preimage[rhs_state] = true;
        state_bijection[lhs_state] = rhs_state;
      }

      const auto
        &lhs_nexts = lhs.transitions[lhs_state],
        &rhs_nexts = rhs.transitions[rhs_state];
      if (lhs_nexts.size() != rhs_nexts.size())
        return false;

      for (auto lhs_next_ : lhs_nexts)
      {
        auto [ch, lhs_next] = lhs_next_;
        auto it = rhs_nexts.find(ch);
        if (it == rhs_nexts.cend())
          return false;
        int rhs_next = it->second;
        stack.push({lhs_next, rhs_next});
      }
    }

    return true;
  }
};

std::istream & operator>>(std::istream &in, DFA &rhs)
{
  rhs.terminals.clear();
  rhs.transitions.clear();

  int n, m, k;
  in >> n >> m >> k;

  for (int i = 0; i < k; i++)
  {
    int t;
    in >> t;
    rhs.terminals.insert(t - 1);
  }

  rhs.transitions.resize(n);
  for (int i = 0; i < m; i++)
  {
    int a, b;
    char c;
    in >> a >> b >> c;
    rhs.transitions[a - 1][c] = b - 1;
  }

  return in;
}

int main()
{
  std::string problem_name = "equivalence";
  std::ifstream in = std::ifstream(problem_name + ".in");
  std::ofstream out = std::ofstream(problem_name + ".out");

  DFA first, second;
  in >> first >> second;
  out << (DFA::are_isomorphic(first.minimize(), second.minimize()) ? "YES" : "NO") << std::endl;
  return 0;
}
