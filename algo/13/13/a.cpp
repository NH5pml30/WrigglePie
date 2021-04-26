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
  }
};

template<class Action>
void walk_edges(const trie &t, Action &&action) {
  for (int v = 0; v < (int)t.nodes.size(); v++)
    for (char ch = 'a'; ch <= 'z'; ch++)
      if (auto u = t.transitions[v][ch - 'a']; u != 0)
        action(v, u, ch);
}

int main() {
  std::string s;
  std::cin >> s;

  std::vector<std::string> dict;
  for (int i = 0; i < (int)s.size(); i++)
    dict.push_back(s.substr(i));
  trie t(dict);
  std::cout << t.nodes.size() << ' ' << t.nodes.size() - 1 << '\n';
  walk_edges(t,
             [](int from, int to, char ch) { std::cout << from + 1 << ' ' << to + 1 << ' ' << ch << '\n'; });
  std::cout.flush();
  return 0;
}
