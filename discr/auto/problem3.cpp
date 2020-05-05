/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <fstream>
#include <stack>

int main()
{
  std::string problem_name = "problem3";
  std::ifstream in = std::ifstream(problem_name + ".in");
  std::ofstream out = std::ofstream(problem_name + ".out");

  int n, m, k;
  in >> n >> m >> k;
  std::stack<int> term_stack;
  std::unordered_set<int> term;
  std::vector<bool> can_terminate(n);
  for (int i = 0; i < k; i++)
  {
    int state;
    in >> state;
    term.insert(state - 1);
    term_stack.push(state - 1);
    can_terminate[state - 1] = true;
  }

  std::vector<std::unordered_map<int, int>> transitions(n);
  std::vector<std::unordered_set<int>> back_transitions(n);

  for (int i = 0; i < m; i++)
  {
    int a, b;
    char c;
    in >> a >> b >> c;
    transitions[a - 1][b - 1]++;
    back_transitions[b - 1].insert(a - 1);
  }

  while (!term_stack.empty())
  {
    int stt = term_stack.top();
    term_stack.pop();
    can_terminate[stt] = true;
    for (auto prev : back_transitions[stt])
      if (!can_terminate[prev])
        term_stack.push(prev);
  }

  if (!can_terminate[0])
  {
    out << 0 << std::endl;
    return 0;
  }

  std::vector<std::pair<int, bool>> noof_words(n);
  std::stack<std::pair<int, bool>> states;
  noof_words[0].first = 1;
  states.push({0, true});
  int result = 0;

  constexpr int mod = 1'000'000'007;

  while (!states.empty())
  {
    auto [stt, begin] = states.top();
    noof_words[stt].second = begin;
    if (!begin)
    {
      states.pop();
      continue;
    }
    else
      states.top().second = false;
    int words = noof_words[stt].first;
    if (term.count(stt))
      result = (result + words) % mod;

    for (auto next : transitions[stt])
      if (can_terminate[next.first])
      {
        if (noof_words[next.first].second)
        {
          out << "-1" << std::endl;
          return 0;
        }
        noof_words[next.first].first = (int)(((long long)words * next.second) % mod);
        states.push({next.first, true});
      }
  }
  out << result << std::endl;
  return 0;
}
