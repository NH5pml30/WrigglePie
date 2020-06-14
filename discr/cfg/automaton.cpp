#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <map>
#include <set>

int main()
{
  std::string problem = "automaton";
  std::ifstream in = std::ifstream(problem + ".in");
  std::ofstream out = std::ofstream(problem + ".out");

  int n;
  char S;
  in >> n >> S;

  std::map<char, std::pair<std::map<char, std::string>, std::string>> rules;

  for (int i = 0; i < n; i++)
  {
    std::string s;
    while (s.empty())
      std::getline(in, s);
    char from, term, not_term;
    if (sscanf(s.c_str(), "%c -> %c%c", &from, &term, &not_term) == 3)
      rules[from].first[term] += not_term;
    else
    {
      sscanf(s.c_str(), "%c -> %c", &from, &term);
      rules[from].second += term;
    }
  }

  int m;
  in >> m;

  for (int i = 0; i < m; i++)
  {
    std::string word;
    in >> word;

    std::set<char> cur_nt = { S };
    for (int pos = 0; pos < word.size() - 1 && !cur_nt.empty(); pos++)
    {
      std::set<char> new_nt;
      for (auto nt : cur_nt)
        new_nt.insert(rules[nt].first[word[pos]].begin(), rules[nt].first[word[pos]].end());
      cur_nt.swap(new_nt);
    }
    bool ok = false;
    for (auto nt : cur_nt)
      if (rules[nt].second.find(word.back()) != std::string::npos)
      {
        out << "yes" << std::endl;
        ok = true;
        break;
      }
    if (!ok)
      out << "no" << std::endl;
  }
  return 0;
}
