/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <array>
#include <map>
#include <set>
#include <stack>

class DFA
{
private:
  std::vector<std::map<char, int>> transitions;
  std::set<int> terminals;

public:
  DFA() { }

  friend std::istream & operator>>(std::istream &i, DFA &rhs);

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
  std::string problem_name = "isomorphism";
  std::ifstream in = std::ifstream(problem_name + ".in");
  std::ofstream out = std::ofstream(problem_name + ".out");

  DFA first, second;
  in >> first >> second;
  out << (DFA::are_isomorphic(first, second) ? "YES" : "NO") << std::endl;

  return 0;
}
