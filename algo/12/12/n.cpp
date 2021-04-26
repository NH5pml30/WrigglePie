/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <limits>
#include <map>
#include <cassert>

struct trie {
  struct node {
    std::vector<int> term_ids;
    bool visited = false;
  };

  std::vector<node> nodes;
  std::vector<int> suffix_refs;
  using transition_table_t = std::map<char, int>;
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
        for (auto trans : table)
          if (trans.second > 0)
            next_layer.insert(trans.second);
      }
      for (int ind : layer)
        func(ind);
      layer.swap(next_layer);
    } while (!layer.empty());
  }

  trie(const std::vector<std::string> &dict, const std::set<char> &alphabet) {
    int root = add_node();

    int s_id = 0;
    for (auto &str : dict) {
      int cur = root;
      int i;
      for (i = 0; i < (int)str.size(); i++)
        if (int to = transitions[cur][str[i]]; to > 0)
          cur = to;
        else
          break;
      for (; i < (int)str.size(); i++)
        cur = transitions[cur][str[i]] = add_node();
      nodes[cur].term_ids.push_back(s_id++);
    }

    bfs([&](int k) {
      for (auto ch : alphabet) {
        if (transitions[k][ch] > 0) {
          if (k != 0)
            suffix_refs[transitions[k][ch]] = transitions[suffix_refs[k]][ch];
        } else {
          transitions[k][ch] = transitions[suffix_refs[k]][ch];
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
      v = transitions[v][ch];
      int u = !nodes[v].term_ids.empty() ? v : supersuffix_refs[v];
      while (!nodes[u].visited && u > 0) {
        nodes[u].visited = true;
        for (auto id : nodes[u].term_ids)
          callback(pos, id);
        u = supersuffix_refs[u];
      }
    }
  }

  bool is_terminal(int node) const {
    return !nodes[node].term_ids.empty();
  }

  bool is_eps_terminal(int node) const {
    return is_terminal(node) || is_terminal(supersuffix_refs[node]);
  }
};

template<auto mod, typename type>
std::vector<std::vector<type>> multiply(const std::vector<std::vector<type>> &lhs, const std::vector<std::vector<type>> &rhs) {
  assert(lhs[0].size() == rhs.size());
  std::vector<std::vector<type>> res(lhs.size());
  for (auto &row : res)
    row.resize(rhs[0].size());

  for (size_t i = 0; i < lhs.size(); i++)
    for (size_t j = 0; j < rhs[0].size(); j++) {
      type sum = 0;
      for (size_t k = 0; k < rhs.size(); k++)
        sum = (sum + lhs[i][k] * rhs[k][j]) % mod;
      res[i][j] = sum;
    }
  return res;
}

template<auto mod, typename type>
void add_assign(std::vector<std::vector<type>> &lhs,
  const std::vector<std::vector<type>> &rhs) {
  assert(lhs.size() == rhs.size() && lhs[0].size() == rhs[0].size());

  for (size_t i = 0; i < lhs.size(); i++)
    for (size_t j = 0; j < rhs.size(); j++)
      lhs[i][j] = (lhs[i][j] + rhs[i][j]) % mod;
}

template<typename OutT, OutT mod>
int count_terminal_words(const trie &T, const std::set<char> &alphabet, int32_t length) {
  std::vector<std::vector<OutT>> matrix(T.nodes.size());

  for (int node = 0; node < (int)T.nodes.size(); node++) {
    matrix[node].resize(T.nodes.size());
    if (T.is_eps_terminal(node))
      matrix[node][node] = (OutT)alphabet.size();  // loop terminals
    else
      for (auto ch : alphabet)
        matrix[node][T.transitions[node].at(ch)]++;
  }

  // matrix^l = matrix^(2^k0 + 2^k1 + ... + 2^ki) = matrix^(2^k0)*matrix^(2^k1)*...*matrix^(2^ki)

  std::vector<std::vector<OutT>> res(T.nodes.size());
  {
    int i = 0;
    for (auto &row : res) {
      row.resize(T.nodes.size());
      row[i++] = 1;
    }
  }
  for (int i = 0; i < 32; i++, matrix = multiply<mod>(matrix, matrix))
    if ((length >> i) & 1)
      res = multiply<mod>(res, matrix);

  int ans = 0;
  for (int node = 0; node < (int)T.nodes.size(); node++)
    ans += T.is_eps_terminal(node) ? res[0][node] : 0;

  return ans % mod;
}

int main() {
  int n;
  std::cin >> n;
  std::vector<std::string> dict(n);
  for (int i = 0; i < n; i++)
    std::cin >> dict[i];

  int32_t l;
  std::cin >> l;

  std::vector<char> alphabet_v('z' - 'a' + 1);
  std::iota(alphabet_v.begin(), alphabet_v.end(), (char)'a');
  std::set<char> alphabet(alphabet_v.begin(), alphabet_v.end());
  trie t(dict, alphabet);

  std::cout << count_terminal_words<int, 10'000>(t, alphabet, l) << std::endl;
  return 0;
}
