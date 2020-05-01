/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <string>
#include <unordered_set>
#include <set>
#include <map>
#include <algorithm>
#include <fstream>

int main()
{
  std::string problem_name = "problem2";
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

  struct trans_t
  {
    int a;
    char c;

    bool operator<(trans_t right) const
    {
      return a < right.a || a == right.a && c < right.c;
    }
  };
  std::vector<std::map<char, std::set<int>>> transitions(n);

  for (int i = 0; i < m; i++)
  {
    int a, b;
    char c;
    in >> a >> b >> c;
    transitions[a - 1][c].insert(b - 1);
  }

  std::set<int> states({0}), back;
  for (auto ch : word)
  {
    back.clear();
    for (auto stt : states)
    {
      auto &vv = transitions[stt][ch];
      back.insert(vv.begin(), vv.end());
    }
    if (back.empty())
    {
      out << "Rejects" << std::endl;
      return 0;
    }
    states.swap(back);
  }
  for (auto stt : states)
  {
    if (term.count(stt))
    {
      out << "Accepts" << std::endl;
      return 0;
    }
  }
  out << "Rejects" << std::endl;
  return 0;
}
