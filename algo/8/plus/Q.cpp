/* Kholiavin Nikolai, M3138 */
#include <algorithm>
#include <functional>
#include <iostream>
#include <numeric>
#include <set>
#include <stack>
#include <vector>
#include <map>

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

  template<class ForwIterator>
  std::vector<bool> solve(int n, ForwIterator begin, ForwIterator end) {
    graph g(n * 2);
    for (; begin != end; begin++) {
      const std::pair<literal, literal> &clause = *begin;
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

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n, m;
  std::cin >> n >> m;

  std::vector<std::pair<two_sat::literal, two_sat::literal>>
      clauses;

  for (int i = 0; i < m; i++) {
    int x, y;
    std::cin >> x >> y;
    x--;
    y--;
    char what;
    std::cin >> what;

    if (what == 'L') {
      // neq
      clauses.push_back({{x, false}, {y, false}});
      clauses.push_back({{x, true}, {y, true}});
    } else {
      // eq
      clauses.push_back({{x, false}, {y, true}});
      clauses.push_back({{x, true}, {y, false}});
    }
  }

  int L = 0, R = clauses.size() / 2;
  // f(L) = 1, f(R + 1) = 0
  while (L < R) {
    int M = (L + R + 1) / 2;
    std::vector<bool> chose = two_sat::solve(n, clauses.begin(), clauses.begin() + M * 2);
    if (!chose.empty())
      // f(M) = 1
      L = M;
    else
      // f(M) = 0
      R = M - 1;
  }

  std::cout << R << std::endl;
  return 0;
}
