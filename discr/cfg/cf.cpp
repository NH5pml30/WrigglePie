/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <cassert>
#include <vector>
#include <fstream>
#include <string>
#include <map>
#include <sstream>
#include <set>
#include <algorithm>
#include <functional>
#include <iterator>

template<typename from, typename to>
std::multimap<from, to> erase_duplicates(const std::multimap<from, to> &Map)
{
  std::multimap<from, to> res;
  for (auto it = Map.begin(); it != Map.end();)
  {
    auto [begin, end] = Map.equal_range(it->first);
    struct proxy
    {
      const from &first;
      const to &second;

      proxy(const from &first, const to &second) : first(first), second(second)
      {
      }

      bool operator<(const proxy &other) const
      {
        return first < other.first || first == other.first && second < other.second;
      }
    };
    std::set<proxy> set;
    for (; begin != end; begin++)
      set.insert({begin->first, begin->second});
    for (auto el : set)
      res.insert({el.first, el.second});
    it = end;
  }
  return res;
}

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

bool CYK(int S, std::map<int, std::set<char>> &rules1,
  std::map<int, std::set<std::pair<int, int>>> &rules2,
  const std::string &w)
{
  std::set<int> all = { S };
  for (auto &r : rules1)
  {
    all.insert(r.first);
    all.insert(r.second.begin(), r.second.end());
  }
  for (auto &r : rules2)
  {
    all.insert(r.first);
    for (auto p : r.second)
      all.insert(p.first), all.insert(p.second);
  }

  std::map<int, std::vector<std::vector<bool>>> dp;
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
            dp[tt][l][r] = dp[tt][l][r] | (dp[rule.first][l][k - 1] & dp[rule.second][k][r]);
  return dp[S][0][w.size() - 1];
}

bool is_terminal(int value)
{
  if (value >= 256 || value < 0)
    return false;
  return std::islower(value);
}

int main()
{
  std::string problem = "cf";
  std::ifstream in = std::ifstream(problem + ".in");
  std::ofstream out = std::ofstream(problem + ".out");

  int n;
  char ch_S;
  in >> n >> ch_S;

  int S = ch_S;
  std::multimap<int, std::vector<int>> rules;
  std::set<int> all = { S };

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
    std::vector<int> str;
    for (auto ch : to)
    {
      if (std::isupper(ch))
        all.insert(ch);
      str.push_back(ch);
    }
    rules.insert({from, str});
  }

  // remove rules with terminals in rhs (except for rules A -> a)
  {
    std::multimap<int, std::vector<int>> new_rules;

    for (auto rule : rules)
    {
      for (auto &ch : rule.second)
        if (is_terminal(ch))
        {
          int guard = ch - 'a' + 256;
          if (all.insert(guard).second)
            new_rules.insert({guard, {ch}});
          ch = guard;
        }
      new_rules.insert(rule);
    }

    rules = erase_duplicates(new_rules);
  }
  int last_nterm = 256 + 25;

  // remove long rules (A -> X_1 X_2 ... X_n)
  {
    std::multimap<int, std::vector<int>> new_rules;
    for (auto rule : rules)
      if (rule.second.size() > 2)
      {
        all.insert(++last_nterm);
        new_rules.insert({rule.first, {rule.second[0], last_nterm}});
        for (int i = 1; i < rule.second.size() - 2; i++, last_nterm++)
        {
          new_rules.insert({last_nterm, {rule.second[i], last_nterm + 1}});
          all.insert(last_nterm + 1);
        }
        new_rules.insert({last_nterm, {rule.second[rule.second.size() - 2], rule.second.back()}});
      }
      else
        new_rules.insert(rule);
    rules = erase_duplicates(new_rules);
  }

  // remove eps-rules (A -> eps)
  {
    std::multimap<int, std::vector<int>> new_rules;

    std::set<int> eps_generating;
    bool changes;
    do
    {
      changes = false;
      for (auto rule : rules)
        if (!eps_generating.count(rule.first) &&
            (rule.second.empty() ||
            rule.second.size() == 1 && eps_generating.count(rule.second[0]) ||
            rule.second.size() == 2 && eps_generating.count(rule.second[0]) &&
              eps_generating.count(rule.second[1])))
        {
          eps_generating.insert(rule.first);
          changes = true;
        }
    } while (changes);

    if (eps_generating.count(S))
    {
      int new_start = ++last_nterm;
      all.insert(new_start);
      new_rules.insert({new_start, {S}});
      new_rules.insert({new_start, {}});
      S = new_start;
    }

    for (auto rule : rules)
    {
      if (rule.second.size() == 2)
      {
        if (eps_generating.count(rule.second[0]))
          new_rules.insert({rule.first, {rule.second[1]}});
        if (eps_generating.count(rule.second[1]))
          new_rules.insert({rule.first, {rule.second[0]}});
      }
      if (!rule.second.empty() || rule.first == S)
        new_rules.insert(rule);
    }

    rules = erase_duplicates(new_rules);
  }

  // remove chain rules (A -> B)
  {
    std::multimap<int, std::vector<int>> new_rules;
    std::set<std::pair<int, int>> chain_pairs;
    for (auto tt : all)
      chain_pairs.insert({tt, tt});
    bool changes;
    do
    {
      changes = false;
      for (auto p : chain_pairs)
        for (auto [it, end] = rules.equal_range(p.second); it != end; ++it)
          if (it->second.size() == 1 && !is_terminal(it->second[0]))
            changes |= chain_pairs.insert({p.first, it->second[0]}).second;
    } while (changes);

    for (auto p : chain_pairs)
      for (auto [it, end] = rules.equal_range(p.second); it != end; ++it)
        if (it->second.size() != 1 || is_terminal(it->second[0]))
          new_rules.insert({p.first, it->second});

    rules = erase_duplicates(new_rules);
  }

  std::string w;
  in >> w;

  std::map<int, std::set<char>> rules1;
  std::map<int, std::set<std::pair<int, int>>> rules2;
  for (auto rule : rules)
  {
    if (rule.second.empty())
      // |w| >= 1, ignore
      assert(rule.first == S);
    else if (rule.second.size() == 1)
    {
      assert(is_terminal(rule.second[0]));
      rules1[rule.first].insert(rule.second[0]);
    }
    else
    {
      assert(rule.second.size() == 2);
      assert(!is_terminal(rule.second[0]));
      assert(!is_terminal(rule.second[1]));
      rules2[rule.first].insert({rule.second[0], rule.second[1]});
    }
  }

  out << (CYK(S, rules1, rules2, w) ? "yes" : "no") << std::endl;
  return 0;
}
