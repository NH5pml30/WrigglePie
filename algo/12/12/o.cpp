/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <map>

struct trie {
  int size = 0;
  std::map<int, std::vector<int>> terminals;
  std::vector<bool> is_terminal;
  std::vector<bool> visited;

  using packed_transition = std::pair<char, int>;
  using transition_table_t = std::vector<packed_transition>;
  mutable std::vector<transition_table_t> transitions;
  mutable std::vector<bool> is_trans_sorted;
  std::vector<std::map<int, std::vector<int>>::const_iterator> supersuffix_refs;

  int get_trans(int from, char ch) const noexcept {
    auto place = std::find_if(transitions[from].begin(), transitions[from].end(),
                              [&](auto pair) { return pair.first == ch; });
    return place == transitions[from].cend() ? 0 : place->second;
  }

  int get_sorted_trans(int from, char ch) const noexcept {
    // if (transitions[from].size() < 5)
    //   return get_trans(from, ch);

    if (!is_trans_sorted[from]) {
      std::sort(transitions[from].begin(), transitions[from].end());
      is_trans_sorted[from] = true;
    }

    auto place = std::lower_bound(transitions[from].begin(), transitions[from].end(), std::make_pair(ch, -1));
    if (place == transitions[from].end() || place->first != ch)
      return 0;
    return place->second;
  }

  int set_trans(int from, char ch, int to) noexcept {
    auto place = std::find_if(transitions[from].begin(), transitions[from].end(),
                              [&](auto pair) { return pair.first == ch; });
    if (place == transitions[from].end()) {
      transitions[from].emplace_back(ch, to);
      place = --transitions[from].end();
    }
    return place->second;
  }

  int add_node() noexcept {
    transitions.emplace_back(transition_table_t{});
    is_terminal.emplace_back(false);
    return size++;
  }

  template<class Func>
  void bfs(Func &&func) noexcept {
    std::set<int> layer = {0};

    do {
      std::set<int> next_layer;
      for (int ind : layer) {
        auto &table = transitions[ind];
        for (auto trans : table)
          if (trans.second > 0)
            next_layer.insert(trans.second);
      }
      for (int ind : layer)
        func(ind);
      layer.swap(next_layer);
    } while (!layer.empty());
  }

  trie(std::vector<std::string> &&dict) noexcept {
    std::vector<int> suffix_refs;
    int root = add_node();

    int s_id = 0;
    for (auto &str : dict) {
      int cur = root;
      int i;
      for (i = 0; i < (int)str.size(); i++)
        if (int to = get_trans(cur, str[i]); to > 0)
          cur = to;
        else
          break;
      for (; i < (int)str.size(); i++)
        cur = set_trans(cur, str[i], add_node());
      is_terminal[cur] = true;
      terminals[cur].push_back(s_id++);
      str.clear();
      str.shrink_to_fit();
    }

    for (auto &t : terminals)
      t.second.shrink_to_fit();
    transitions.shrink_to_fit();
    is_trans_sorted.resize(transitions.size());
    suffix_refs.resize(size, 0);
    supersuffix_refs.resize(size, terminals.cend());
    visited.resize(size, false);

    std::vector<bool> set_ch('z' - 'a' + 1);

    bfs([&](int k) {
      std::fill(set_ch.begin(), set_ch.end(), false);

      if (k != 0)
        for (auto [ch, to] : transitions[k]) {
          suffix_refs[to] = get_trans(suffix_refs[k], ch);
          set_ch[ch - 'a'] = true;
        }

      for (auto [ch, to] : transitions[suffix_refs[k]])
        if (!set_ch[ch - 'a'])
          set_trans(k, ch, to);

      if (is_terminal[suffix_refs[k]]) {
        supersuffix_refs[k] = terminals.find(suffix_refs[k]);
      } else {
        supersuffix_refs[k] = supersuffix_refs[suffix_refs[k]];
      }
    });
  }

  template<class Callback>
  void run(std::string_view sv, Callback &&callback) noexcept {
    std::fill(visited.begin(), visited.end(), false);

    int v = 0;

    for (auto ch : sv) {
      v = get_sorted_trans(v, ch);
      auto u = is_terminal[v] ? terminals.find(v) : supersuffix_refs[v];
      while (u != terminals.end() && !visited[u->first]) {
        visited[u->first] = true;
        for (auto id : u->second)
          callback(id);
        u = supersuffix_refs[u->first];
      }
    }
  }
};

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n;
  std::cin >> n;
  std::vector<std::string> dict(n);
  std::for_each(dict.begin(), dict.end(), [](auto &str) { std::cin >> str; });
  trie t(std::move(dict));
  std::vector<int> ans(n);

  int q;
  std::cin >> q;
  std::string p;

  for (int i = 0; i < q; i++) {
    int z;
    std::cin >> z;

    if (z == 1) {
      std::cin >> p;
      t.run(p, [&](int string_id) { ans[string_id]++; });
    } else {
      int s;
      std::cin >> s;
      std::cout << ans[s - 1] << '\n';
    }
  }

  std::cout.flush();
  return 0;
}
