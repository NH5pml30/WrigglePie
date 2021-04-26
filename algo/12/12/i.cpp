/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>

struct trie {
  struct node {
    std::vector<int> term_ids;
    bool visited = false;
  };

  std::vector<node> nodes;
  std::vector<int> suffix_refs;
  using transition_table_t = std::array<int, 'z' - 'a' + 1>;
  std::vector<transition_table_t> transitions;
  std::vector<int> supersuffix_refs;

  int add_node() {
    nodes.push_back({{}});
    suffix_refs.push_back(0);
    supersuffix_refs.push_back(0);
    transitions.emplace_back(transition_table_t{});
    return (int)nodes.size() - 1;
  }

  template<class Func>
  void bfs(Func &&func) {
    std::set<int> layer = {0};

    do {
      std::set<int> next_layer;
      for (int ind : layer) {
        auto &table = transitions[ind];
        for (int to : table)
          if (to > 0)
            next_layer.insert(to);
      }
      for (int ind : layer)
        func(ind);
      layer.swap(next_layer);
    } while (!layer.empty());
  }

  trie(const std::vector<std::string> &dict) {
    int root = add_node();

    int s_id = 0;
    for (auto &str : dict) {
      int cur = root;
      int i;
      for (i = 0; i < (int)str.size(); i++)
        if (int to = transitions[cur][str[i] - 'a']; to > 0)
          cur = to;
        else
          break;
      for (; i < (int)str.size(); i++)
        cur = transitions[cur][str[i] - 'a'] = add_node();
      nodes[cur].term_ids.push_back(s_id++);
    }

    bfs([&](int k) {
      for (auto ch = 'a'; ch <= 'z'; ch++) {
        if (transitions[k][ch - 'a'] > 0) {
          if (k != 0)
            suffix_refs[transitions[k][ch - 'a']] = transitions[suffix_refs[k]][ch - 'a'];
        } else {
          transitions[k][ch - 'a'] = transitions[suffix_refs[k]][ch - 'a'];
        }
      }

      if (!nodes[suffix_refs[k]].term_ids.empty()) {
        supersuffix_refs[k] = suffix_refs[k];
      } else {
        supersuffix_refs[k] = supersuffix_refs[suffix_refs[k]];
      }
    });
  }

  template<class Callback>
  void run(std::string_view sv, Callback &&callback) {
    int v = 0;

    int pos = 0;
    for (auto ch : sv) {
      v = transitions[v][ch - 'a'];
      int u = !nodes[v].term_ids.empty() ? v : supersuffix_refs[v];
      while (!nodes[u].visited && u > 0) {
        nodes[u].visited = true;
        for (auto id : nodes[u].term_ids)
          callback(pos, id);
        u = supersuffix_refs[u];
      }
    }
  }
};

int main() {
  std::string text;
  std::cin >> text;

  int n;
  std::cin >> n;
  std::vector<std::string> dict(n);
  std::for_each(dict.begin(), dict.end(), [](auto &str) { std::cin >> str; });

  trie t(dict);

  std::vector<bool> res(n);
  t.run(text, [&](int, int string_id) { res[string_id] = true; });

  std::for_each(res.begin(), res.end(), [](bool r) { std::cout << (r ? "Yes" : "No") << '\n'; });
  std::cout.flush();
  return 0;
}
