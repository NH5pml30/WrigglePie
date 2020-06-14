#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <map>
#include <sstream>
#include <set>
#include <algorithm>

int main()
{
  std::string problem = "epsilon";
  std::ifstream in = std::ifstream(problem + ".in");
  std::ofstream out = std::ofstream(problem + ".out");

  int n;
  char S;
  in >> n >> S;

  std::map<char, std::set<std::string>> rules;
  std::vector<bool> is_eps(std::numeric_limits<char>::max() + 1);

  for (int i = 0; i < n; i++)
  {
    std::string s;
    while (s.empty())
      std::getline(in, s);
    std::stringstream ss(s);
    char from;
    std::string arrow, to;
    ss >> from >> arrow;
    if (!(ss >> to))
      is_eps[from] = true;
    if (std::all_of(to.begin(), to.end(), std::isupper))
      rules[from].insert(to);
  }

  bool changes;
  do
  {
    changes = false;
    for (auto rule : rules)
      if (!is_eps[rule.first])
        if (std::any_of(rule.second.begin(), rule.second.end(),
            [&](const std::string &str)
            {
              return std::all_of(str.begin(), str.end(), [&](char tt) { return is_eps[tt]; });
            }))
        {
          is_eps[rule.first] = true;
          changes = true;
        }
  } while (changes);

  for (char tt = 'A'; tt <= 'Z'; tt++)
    if (is_eps[tt])
      out << tt << ' ';
  out << std::endl;
  return 0;
}
