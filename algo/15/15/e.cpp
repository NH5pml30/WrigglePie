/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <cassert>
#include <stack>
#include <optional>
#include <variant>
#include <sstream>
#include <random>
#include <bitset>
#include <list>
#include <iterator>

namespace two_sat {
  struct graph {
    int n;
    std::vector<std::set<int>> edges;

    graph(int n) : n(n), edges(n) {}
    void add_edge(int from, int to) {
      edges[from].insert(to);
    }

    graph reverse() {
      graph res(n);
      for (int to = 0; to < n; to++)
        for (auto from : edges[to])
          res.add_edge(from, to);
      return res;
    }

    std::vector<int> condense() {
      graph rev = reverse();

      std::vector<int> topo_sorted;
      topo_sorted.reserve(n);

      std::vector<bool> used(n, false);
      std::function<void(int)> dfs1 = [&](int start) {
        std::vector<std::pair<int, std::set<int>::iterator>> st;
        st.push_back({start, edges[start].begin()});
        while (!st.empty()) {
          int v = st.back().first;
          auto next_child = st.back().second;
          st.pop_back();
          used[v] = true;
          while (next_child != edges[v].end() && used[*next_child])
            next_child++;
          if (next_child != edges[v].end()) {
            st.push_back({v, std::next(next_child)});
            st.push_back({*next_child, edges[*next_child].begin()});
          } else {
            topo_sorted.push_back(v);
          }
        }
      };

      for (int i = 0; i < n; i++)
        if (!used[i])
          dfs1(i);

      std::vector<int> results(n, -1);
      int comp_id = 0;
      std::fill(used.begin(), used.end(), false);

      std::function<void(int)> dfs2 = [&](int start) {
        std::vector<int> st;
        st.push_back(start);
        while (!st.empty()) {
          int v = st.back();
          st.pop_back();
          used[v] = true;
          results[v] = comp_id;
          for (int u : rev.edges[v])
            if (!used[u] && results[u] == -1)
              st.push_back(u);
        }
      };

      for (int i = (int)topo_sorted.size() - 1; i >= 0; i--)
        if (int node = topo_sorted[i]; results[node] == -1) {
          dfs2(node);
          comp_id++;
        }

      return results;
    }
  };

  struct literal {
    int i;
    bool inverse_flag;

    literal inverse() {
      return {i, !inverse_flag};
    }
  };

  std::vector<bool> solve(
      int n, const std::vector<std::pair<literal, literal>> &clauses) {
    graph g(n * 2);
    for (auto &clause : clauses) {
      g.add_edge(
        clause.first.i + (!clause.first.inverse_flag) * n,
        clause.second.i + clause.second.inverse_flag * n);
      g.add_edge(
        clause.second.i + (!clause.second.inverse_flag) * n,
        clause.first.i + clause.first.inverse_flag * n);
    }

    std::vector<int> id2comp = g.condense();
    std::vector<bool> res(n);
    for (int i = 0; i < n; i++) {
      if (id2comp[i] == id2comp[n + i]) return {};
      if (id2comp[i] > id2comp[n + i])
        res[i] = true;
      else
        res[i] = false;
    }
    return res;
  }
}

enum class color {
  RED, GREEN, BLUE
};

std::ostream &operator<<(std::ostream &o, color c) {
  switch (c) {
    case color::RED:
      return o << 'R';
    case color::GREEN:
      return o << 'G';
    case color::BLUE:
      return o << 'B';
  }
  return o;
}

std::istream &operator>>(std::istream &i, color &c) {
  char ch;
  i >> ch;

  c = ch == 'R' ? color::RED : ch == 'G' ? color::GREEN : color::BLUE;
  return i;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto &&in = std::cin;

  int n, m;
  in >> n >> m;

  std::vector<color> colors(n);
  for (int i = 0; i < n; i++)
    in >> colors[i];

  auto shift_color = [](color c, int diff = 1) {
    return static_cast<color>((static_cast<int>(c) + diff) % 3);
  };

  using table_t = std::array<bool, 4>;

  auto table_to_clauses = [](table_t table, int x, int y) {
    std::vector<std::pair<two_sat::literal, two_sat::literal>> result;
    if (!table[0b00])
      // mask with x || y
      result.push_back({{x, false}, {y, false}});
    if (!table[0b01])
      // mask with x || !y
      result.push_back({{x, false}, {y, true}});
    if (!table[0b10])
      // mask with !x || y
      result.push_back({{x, true}, {y, false}});
    if (!table[0b11])
      // mask with !x || !y
      result.push_back({{x, true}, {y, true}});
    return result;
  };

  std::vector<std::pair<two_sat::literal, two_sat::literal>> formula;
  for (int i = 0; i < m; i++) {
    int u, v;
    in >> u >> v;
    u--, v--;
    table_t table{};
    // false -> shift by one
    // true  -> shift by two
    table[0b00] = shift_color(colors[u], 1) != shift_color(colors[v], 1);
    table[0b01] = shift_color(colors[u], 1) != shift_color(colors[v], 2);
    table[0b10] = shift_color(colors[u], 2) != shift_color(colors[v], 1);
    table[0b11] = shift_color(colors[u], 2) != shift_color(colors[v], 2);

    auto clauses = table_to_clauses(table, u, v);
    formula.insert(formula.end(), clauses.begin(), clauses.end());
  }

  auto values = two_sat::solve(n, formula);
  if (values.empty()) {
    std::cout << "Impossible" << std::endl;
    return 0;
  }

  for (int v = 0; v < n; v++)
    std::cout << shift_color(colors[v], 1 + values[v]);
  std::cout << std::endl;
  return 0;
}
