/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <string>
#include <unordered_set>
#include <array>
#include <fstream>

int main()
{
  std::string problem_name = "problem1";
  std::ifstream in = std::ifstream(problem_name + ".in");
  std::ofstream out = std::ofstream(problem_name + ".out");

  std::string word;
  in >> word;

  int n, m, k;
  in >> n >> m >> k;
  std::unordered_set<int> term;
  for (int i = 0; i < k; i++)
  {
    int state;
    in >> state;
    term.insert(state - 1);
  }

  std::vector<std::array<int, 26>> transitions(n);
  for (int i = 0; i < m; i++)
  {
    int a, b;
    char c;
    in >> a >> b >> c;
    if (a - 1 >= transitions.size())
      transitions.resize(a);
    transitions[a - 1][c - 'a'] = b;
  }

  int state = 0;
  for (auto ch : word)
  {
    state = transitions[state][ch - 'a'] - 1;
    if (state == -1)
    {
      out << "Rejects" << std::endl;
      return 0;
    }
  }
  if (term.count(state))
    out << "Accepts" << std::endl;
  else
    out << "Rejects" << std::endl;
  return 0;
}
