/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <map>
#include <sstream>
#include <set>
#include <algorithm>
#include <functional>
#include <iterator>

int main()
{
  std::string problem = "nfc";
  std::ifstream in = std::ifstream(problem + ".in");
  std::ofstream out = std::ofstream(problem + ".out");

  int n;
  char S;
  in >> n >> S;

  std::map<char, std::set<char>> rules1;
  std::map<char, std::set<std::pair<char, char>>> rules2;
  std::set<char> all = {S};

  for (int i = 0; i < n; i++)
  {
    std::string s;
    while (s.empty())
      std::getline(in, s);
    std::stringstream ss(s);
    char from;
    std::string arrow, to;
    ss >> from >> arrow >> to;
    all.insert(from);
    if (to.size() == 1)
      rules1[from].insert(to[0]);
    else
    {
      rules2[from].insert({to[0], to[1]});
      all.insert(to[0]);
      all.insert(to[1]);
    }
  }

  std::string w;
  in >> w;
  constexpr int mod = 1'000'000'007;

  std::map<char, std::vector<std::vector<int>>> dp;
  for (auto tt : all)
  {
    dp[tt].resize(w.size());
    for (int i = 0; i < w.size(); i++)
    {
      dp[tt][i].resize(w.size());
      dp[tt][i][i] = rules1[tt].count(w[i]) > 0;
    }
  }

  for (int len = 2; len <= w.size(); len++)
    for (int l = 0, r = len - 1; r < w.size(); l++, r++)
      for (auto tt : all)
        for (auto rule : rules2[tt])
          for (int k = l + 1; k <= r; k++)
          {
            int res = (int)(((long long)dp[rule.first][l][k - 1] * dp[rule.second][k][r]) % mod);
            dp[tt][l][r] = (dp[tt][l][r] + res) % mod;
          }
  auto res = dp[S][0][w.size() - 1];
  out << res << std::endl;
  return 0;
}
