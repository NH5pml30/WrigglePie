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

template<typename from, typename to, class pred>
  void remove_if(std::multimap<from, to> &Map, pred Pred)
  {
    for (auto it = Map.begin(); it != Map.end();)
      if (Pred(*it))
      {
        auto save = std::next(it);
        Map.erase(it);
        it = save;
      }
      else
        ++it;
  }

int main()
{
  std::string problem = "useless";
  std::ifstream in = std::ifstream(problem + ".in");
  std::ofstream out = std::ofstream(problem + ".out");

  int n;
  char S;
  in >> n >> S;

  std::multimap<char, std::string> rules;
  std::set<char> goes_to_terminal, generating, all;
  all.insert(S);

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
    if (std::none_of(to.begin(), to.end(), std::isupper))
      goes_to_terminal.insert(from);
    else
    {
      std::string new_to;
      for (auto ch : to)
        if (std::isupper(ch))
        {
          new_to += ch;
          all.insert(ch);
        }
      rules.insert({from, new_to});
    }
  }
  generating = goes_to_terminal;

  bool changes;
  do
  {
    changes = false;
    for (auto &rule : rules)
      if (generating.count(rule.first) == 0)
        if (std::all_of(rule.second.begin(), rule.second.end(), [&](char tt)
              {
                return generating.count(tt) > 0;
              }))
        {
          generating.insert(rule.first);
          changes = true;
        }
  } while (changes);
  remove_if(rules, [&](auto a)
    {
      return std::any_of(a.second.begin(), a.second.end(),
        [&](char ch) { return generating.count(ch) == 0; });
    });

  std::set<char> reachable;
  std::function<void (char, std::set<char> &)> dfs = [&](char tt, std::set<char> &container)
  {
    container.insert(tt);
    for (auto [it, end] = rules.equal_range(tt); it != end; ++it)
      for (auto next : it->second)
        if (container.count(next) == 0)
          dfs(next, container);
  };
  dfs(S, reachable);
  remove_if(rules, [&](auto a) { return reachable.count(a.first) == 0; });

  std::set<char> needed;
  std::set_intersection(reachable.begin(), reachable.end(),
    goes_to_terminal.begin(), goes_to_terminal.end(),
    std::inserter(needed, needed.begin()));
  for (auto &rule : rules)
  {
    needed.insert(rule.first);
    std::copy(rule.second.begin(), rule.second.end(), std::inserter(needed, needed.begin()));
  }

  for (auto tt : all)
    if (needed.count(tt) == 0)
      out << tt << ' ';
  out << std::endl;
  return 0;
}
