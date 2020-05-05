/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <fstream>
#include <cstring>

int main()
{
  std::string problem_name = "problem4";
  std::ifstream in = std::ifstream(problem_name + ".in");
  std::ofstream out = std::ofstream(problem_name + ".out");

  int n, m, k, l;
  in >> n >> m >> k >> l;
  std::unordered_set<int> recalc;
  std::vector<int> noof_words(n);
  for (int i = 0; i < k; i++)
  {
    int state;
    in >> state;
    recalc.insert(state - 1);
    noof_words[state - 1] = 1;
  }

  std::vector<std::unordered_map<int, int>> transitions(n);

  for (int i = 0; i < m; i++)
  {
    int a, b;
    char c;
    in >> a >> b >> c;
    transitions[b - 1][a - 1]++;
  }

  constexpr int mod = 1'000'000'007;

  std::unordered_set<int> back;
  std::vector<int> noof_words_back(n);
  for (int i = 0; i < l && !recalc.empty(); i++)
  {
    back.clear();
    memset(noof_words_back.data(), 0, sizeof(int) * noof_words_back.size());
    for (int stt : recalc)
    {
      long long words = noof_words[stt];
      for (auto prev : transitions[stt])
      {
        noof_words_back[prev.first] = (int)((noof_words_back[prev.first] + words * prev.second) % mod);
        back.insert(prev.first);
      }
    }
    recalc.swap(back);
    noof_words.swap(noof_words_back);
  }

  out << noof_words[0] << std::endl;
  return 0;
}
