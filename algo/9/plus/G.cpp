/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <iostream>
#include <limits>
#include <queue>
#include <list>
#include <tuple>
#include <vector>
#include <set>
#include <cmath>
#include <iomanip>
#include <optional>

class disjoint_set {
 private:
  struct NODE_DATA {
    short index, parent, rank;

    NODE_DATA(short index) :
      index(index), parent(index), rank(0) {
    }

    NODE_DATA() {
    }

    static void attach(NODE_DATA &X, NODE_DATA &Y) {
      if (X.rank > Y.rank) {
        Y.parent = X.index;
      } else {
        X.parent = Y.index;
        if (X.rank == Y.rank)
          Y.rank++;
      }
    }
  };

  std::vector<NODE_DATA> nodes;

 public:
  disjoint_set(int n) : nodes(n) {
    short i = 0;
    for (auto &el : nodes)
      el = NODE_DATA(i++);
  }

  short find(short x) {
    if (nodes[x].parent == x)
      return x;
    return nodes[x].parent = find(nodes[x].parent);
  }

  void join(short x, short y) {
    x = find(x), y = find(y);
    if (x == y)
      return;

    NODE_DATA::attach(nodes[x], nodes[y]);
  }
};

struct empty {};

template<class EDGE_DATA = empty>
class full_graph {
 private:
  int n;

  struct EDGE {
    short from, to;
    EDGE_DATA data;
  };
  std::vector<EDGE> edges;

 public:
  full_graph() {}

  void resize(size_t size) {
    n = (int)size;
    edges.resize(n * (n - 1) / 2);
    size_t ind = 0;
    for (int i = 0; i < n; i++)
      for (int j = 0; j < i; j++) edges[ind++] = {(short)i, (short)j, {}};
  }

  size_t size() const { return n; }

  void set_edge(int from, int to, const EDGE_DATA &data) {
    if (from < to) std::swap(from, to);
    edges[from * (from - 1) / 2 + to].data = data;
  }

  using weight_t = int;
  std::enable_if_t<std::is_convertible_v<EDGE_DATA, weight_t>, double> mst() {
    // Kruskal
    auto comparator = [&](const EDGE &left, const EDGE &right) {
      return left.data < right.data;
    };
    std::sort(edges.begin(), edges.end(), comparator);

    disjoint_set comps(n);
    double res = 0;
    for (auto &uw : edges) {
      if (comps.find(uw.from) != comps.find(uw.to)) {
        // Edge is safe, add to mst
        comps.join(uw.from, uw.to);
        res += sqrt((int)uw.data);
      }
    }

    return res;
  }
};

struct grid_dist {
  short dx, dy;

  operator int() const { return (int)dx * dx + (int)dy * dy; }
};

using EDGE_DATA_T = grid_dist;
using graph_t = full_graph<EDGE_DATA_T>;

graph_t read_graph(std::istream &in) {
  int n;
  in >> n;
  graph_t g;
  g.resize(n);

  std::vector<std::pair<short, short>> v(n);
  for (int i = 0; i < n; i++)
    in >> v[i].first >> v[i].second;

  for (int i = 0; i < n; i++)
    for (int j = 0; j < i; j++)
      g.set_edge(i, j, {(short)(v[i].first - v[j].first), (short)(v[i].second - v[j].second)});
  return std::move(g);
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  graph_t g = read_graph(std::cin);
  std::cout << std::setprecision(std::numeric_limits<double>::digits10 + 1) <<
    g.mst() << std::endl;
  return 0;
}
