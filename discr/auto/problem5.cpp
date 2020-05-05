/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <fstream>
#include <cstring>
#include <stack>
#include <array>
#include <bitset>

int count_words(int n, std::unordered_set<int> term,
                const std::unordered_map<int, std::array<int, 26>> &forw_trans, int l)
{
  /// REVERT TRANSITIONS
  std::vector<std::unordered_map<int, int>> transitions(n);
  for (auto trans : forw_trans)
    for (char ch = 'a'; ch <= 'z'; ch++)
      if (trans.second[ch - 'a'] != 0)
        transitions[trans.second[ch - 'a'] - 1][trans.first]++;

  std::vector<int> noof_words(n);
  for (auto state : term)
    noof_words[state] = 1;

  constexpr int mod = 1'000'000'007;

  std::unordered_set<int> back;
  std::vector<int> noof_words_back(n);
  for (int i = 0; i < l && !term.empty(); i++)
  {
    back.clear();
    memset(noof_words_back.data(), 0, sizeof(int) * noof_words_back.size());
    for (int stt : term)
    {
      long long words = noof_words[stt];
      for (auto prev : transitions[stt])
      {
        noof_words_back[prev.first] = (int)((noof_words_back[prev.first] + words * prev.second) % mod);
        back.insert(prev.first);
      }
    }
    term.swap(back);
    noof_words.swap(noof_words_back);
  }

  return noof_words[0];
}

int main()
{
  std::string problem_name = "problem5";
  std::ifstream in = std::ifstream(problem_name + ".in");
  std::ofstream out = std::ofstream(problem_name + ".out");

  int n, m, k, l;
  in >> n >> m >> k >> l;
  std::bitset<100> term;
  for (int i = 0; i < k; i++)
  {
    int state;
    in >> state;
    term.set(state - 1, true);
  }

  std::vector<std::array<std::vector<int>, 26>> transitions(n);

  for (int i = 0; i < m; i++)
  {
    int a, b;
    char c;
    in >> a >> b >> c;
    transitions[a - 1][c - 'a'].push_back(b - 1);
  }

  using state_t = std::bitset<100>;

  std::unordered_map<state_t, int> old2new;
  old2new.insert({1, 0});

  std::unordered_map<int, std::array<int, 26>> new_transitions;
  std::unordered_set<int> new_term;

  auto get_state_id = [&](const state_t &stt) -> std::pair<int, bool>
  {
    if (auto it = old2new.find(stt); it != old2new.cend())
      return {it->second, false};
    old2new.insert({stt, (int)old2new.size()});
    return {(int)(old2new.size() - 1), true};
  };

  std::stack<std::pair<state_t, int>> states;
  states.push({1, 0});
  while (!states.empty())
  {
    auto [stt, id] = states.top();
    states.pop();
    for (char ch = 'a'; ch <= 'z'; ch++)
    {
      state_t next_stt;
      for (int i = 0; i < n; i++)
        if (stt.test(i))
          for (auto next : transitions[i][ch - 'a'])
            next_stt.set(next, true);
      if (next_stt.any())
      {
        auto [next_id, is_new_state] = get_state_id(next_stt);
        new_transitions[id][ch - 'a'] = next_id + 1;
        if (is_new_state)
        {
          if ((next_stt & term).any())
            new_term.insert(next_id);
          states.push({next_stt, next_id});
        }
      }
    }
  }

  out << count_words((int)old2new.size(), new_term, new_transitions, l) << std::endl;
  return 0;
}
