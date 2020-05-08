/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <array>
#include <stack>
#include <queue>
#include <set>
#include <map>
#include <unordered_set>
#include <unordered_map>

std::unordered_set<int> inverse_set(const std::unordered_set<int> &set, int n)
{
  std::set<int> temp(set.begin(), set.end());
  std::unordered_set<int> res;
  auto it = temp.cbegin();
  for (int v = 0; v < n; v++)
    if (it == temp.cend() || v < *it)
      res.insert(v);
    else
      it++;
  return res;
}

class DFA
{
private:
  std::vector<std::unordered_map<char, int>> transitions;
  std::unordered_set<int> terminals;

public:
  DFA() { }

  friend std::istream & operator>>(std::istream &i, DFA &rhs);
  friend std::ostream & operator<<(std::ostream &out, const DFA &rhs);

  DFA & throw_out_unreachable()
  {
    int n = (int)transitions.size();
    std::vector<bool> is_reachable(n);
    std::stack<int> stack;
    stack.push(0);
    int n_of_reachable = 0;
    while (!stack.empty())
    {
      int stt = stack.top();
      stack.pop();
      is_reachable[stt] = true;
      n_of_reachable++;
      for (auto next : transitions[stt])
        if (!is_reachable[next.second])
          stack.push(next.second);
    }

    std::vector<std::unordered_map<char, int>> new_trans(n_of_reachable);
    std::unordered_set<int> new_terms;
    std::vector<int> fake2real(n);

    int ind = 0;
    for (int i = 0; i < n; i++)
      if (is_reachable[i])
        fake2real[i] = ind++;

    ind = 0;
    for (int i = 0; i < n; i++)
      if (is_reachable[i])
      {
        for (auto next : transitions[i])
          new_trans[ind][next.first] = fake2real[next.second];

        if (terminals.count(i))
          new_terms.insert(ind);
        ind++;
      }

    terminals.swap(new_terms);
    transitions.swap(new_trans);
    return *this;
  }

private:
  std::pair<std::pair<char, int>, int> place_unterminatable()
  {
    int n = (int)transitions.size();
    std::queue<int> queue;
    std::vector<bool> was(n);
    queue.push({0});
    while (!queue.empty())
    {
      int stt = queue.front();
      was[stt] = true;
      queue.pop();
      if (transitions[stt].size() != 'z' - 'a' + 1)
        for (char ch = 'a'; ch <= 'z'; ch++)
          if (transitions[stt].count(ch) == 0)
          {
            transitions[stt][ch] = n;
            transitions.push_back({});
            return {{ch, stt}, n};
          }
      for (auto next : transitions[stt])
        if (!was[next.second])
          queue.push(next.second);
    }
    return {{'\0', -1}, -1};
  }

public:
  DFA & fast_minimize()
  {
    throw_out_unreachable();
    auto [trash_trans, trash] = place_unterminatable();

    int n = (int)transitions.size();
    std::vector<std::unordered_map<char, std::set<int>>> back_transitions(n);
    for (int i = 0; i < n; i++)
      for (auto next : transitions[i])
        back_transitions[next.second][next.first].insert(i);

    std::array<std::unordered_set<int>, 'z' - 'a' + 1> goes2trash;
    if (trash_trans.second != -1)
      goes2trash[trash_trans.first - 'a'].insert(trash_trans.second);
    for (int i = 0; i < n; i++)
      if (transitions[i].size() != ('z' - 'a' + 1))
        for (char ch = 'a'; ch <= 'z'; ch++)
          if (transitions[i].count(ch) == 0)
            goes2trash[ch - 'a'].insert(i);

    std::vector<std::unordered_set<int>> partition(2);
    partition[0] = terminals;
    partition[1] = inverse_set(terminals, n);

    struct pair_hash
    {
      inline size_t operator()(const std::pair<int, char> & v) const
      {
        return v.first * ('z' - 'a' + 1) + (v.second - 'a');
      }
    };

    std::vector<std::pair<int, char>> queue;
    for (char ch = 'a'; ch <= 'z'; ch++)
    {
      queue.push_back({0, ch});
      queue.push_back({1, ch});
    }

    std::vector<int> stt2class(n, 1);
    for (auto stt : terminals)
      stt2class[stt] = 0;

    while (!queue.empty())
    {
      auto [splitter_id, ch] = queue.back();
      queue.pop_back();
      std::unordered_set<int> splitter = partition[splitter_id];

      std::unordered_map<int, std::unordered_set<int>> adj_to_splitter;
      for (auto stt : splitter)
        if (auto it = back_transitions[stt].find(ch); it != back_transitions[stt].end())
          for (auto prev : it->second)
          {
            int splittee_id = stt2class[prev];
            auto &splittee = partition[splittee_id];
            if (auto it = splittee.find(prev); it != splittee.end())
              adj_to_splitter[splittee_id].insert(splittee.extract(it));
          }
      if (trash != -1 && splitter_id == stt2class[trash])
        for (auto stt : goes2trash[ch - 'a'])
        {
          int splittee_id = stt2class[stt];
          auto &splittee = partition[splittee_id];
          if (auto it = splittee.find(stt); it != splittee.end())
            adj_to_splitter[splittee_id].insert(splittee.extract(it));
        }

      for (auto &splittee_ : adj_to_splitter)
      {
        int splittee_id = splittee_.first;
        auto &separated = splittee_.second;
        if (partition[splittee_id].size() == 0)
        {
          partition[splittee_id].swap(splittee_.second);
          continue;
        }

        int other_id = (int)partition.size();
        partition.emplace_back(std::move(separated));
        auto &other = partition[other_id], &splittee = partition[splittee_id];
        if (splittee.size() < other.size())
          splittee.swap(other);
        for (auto stt : other)
          stt2class[stt] = other_id;

        for (char ch = 'a'; ch <= 'z'; ch++)
          queue.push_back({other_id, ch});
      }
    }

    std::vector<std::unordered_map<char, int>> new_trans(partition.size());
    std::unordered_set<int> new_terms;

    if (int other_id = stt2class[0]; other_id != 0)
    {
      for (auto el : partition[other_id])
        stt2class[el] = 0;
      for (auto el : partition[0])
        stt2class[el] = other_id;
      partition[0].swap(partition[other_id]);
    }

    for (int start = 0; start < n; start++)
      for (auto trans : transitions[start])
        new_trans[stt2class[start]][trans.first] = stt2class[trans.second];
    for (auto term : terminals)
      new_terms.insert(stt2class[term]);

    transitions.swap(new_trans);
    terminals.swap(new_terms);

    return remove_unterminatable();
  }

  DFA & remove_unterminatable()
  {
    int n = (int)transitions.size();
    std::vector<std::unordered_map<char, std::unordered_set<int>>> back_transitions(n);

    for (int i = 0; i < n; i++)
      for (auto next : transitions[i])
        back_transitions[next.second][next.first].insert(i);

    std::vector<bool> terminatable(n);
    std::queue<int> queue;
    for (auto stt : terminals)
      queue.push(stt);

    while (!queue.empty())
    {
      int stt = queue.front();
      queue.pop();

      terminatable[stt] = true;

      for (auto &prev_ : back_transitions[stt])
        for (auto prev : prev_.second)
          if (!terminatable[prev])
            queue.push(prev);
    }

    std::vector<int> stt2newstt(n);
    int cur_id = 0;
    for (int i = 0; i < n; i++)
      if (terminatable[i])
        stt2newstt[i] = cur_id++;

    std::vector<std::unordered_map<char, int>> new_trans;
    std::unordered_set<int> new_term;
    new_trans.reserve(cur_id);
    for (int i = 0; i < n; i++)
      if (terminatable[i])
      {
        new_trans.emplace_back();
        auto &new_tr = new_trans.back();
        for (auto tr : transitions[i])
          if (terminatable[tr.second])
            new_tr.insert({tr.first, stt2newstt[tr.second]});
      }
    for (auto stt : terminals)
      new_term.insert(stt2newstt[stt]);

    transitions.swap(new_trans);
    terminals.swap(new_term);

    return *this;
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

std::ostream & operator<<(std::ostream &out, const DFA &rhs)
{
  out << rhs.transitions.size() << ' ';
  int m = 0;
  for (auto tr : rhs.transitions)
    m += (int)tr.size();
  out << m << ' ' << rhs.terminals.size() << std::endl;

  for (auto term : rhs.terminals)
    out << term + 1 << ' ';
  out << std::endl;

  for (int start = 0; start < (int)rhs.transitions.size(); start++)
    for (auto next : rhs.transitions[start])
      out << start + 1 << ' ' << next.second + 1 << ' ' << next.first << std::endl;

  return out;
}

int main()
{
  std::string problem_name = "fastminimization";
  std::ifstream in = std::ifstream(problem_name + ".in");
  std::ofstream out = std::ofstream(problem_name + ".out");

  DFA automaton;
  in >> automaton;
  out << automaton.fast_minimize();
  return 0;
}
