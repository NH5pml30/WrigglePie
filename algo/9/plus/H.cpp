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
    int index, parent, rank;

    NODE_DATA(int index) :
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
    int i = 0;
    for (auto &el : nodes)
      el = NODE_DATA(i++);
  }

  int find(int x) {
    if (nodes[x].parent == x)
      return x;
    return nodes[x].parent = find(nodes[x].parent);
  }

  void join(int x, int y) {
    x = find(x), y = find(y);
    if (x == y)
      return;

    NODE_DATA::attach(nodes[x], nodes[y]);
  }
};

template<typename T>
constexpr T infinity = std::numeric_limits<T>::max();

struct empty {};
template <class NODE_DATA = empty, class EDGE_DATA = empty>
class graph {
 private:
  struct NODE {
    struct EDGE_ENTRY {
      int other_id;
      EDGE_DATA data;

      EDGE_ENTRY() {}
      EDGE_ENTRY(int other_id, const EDGE_DATA &data)
          : other_id(other_id), data(data) {}
      EDGE_ENTRY(int other_id, EDGE_DATA &&data)
          : other_id(other_id), data(std::move(data)) {}

      bool operator<(const EDGE_ENTRY &other) const {
        return data < other.data;
      }
    };
    int id;
    NODE_DATA data;
    std::vector<EDGE_ENTRY> adjacent;

    NODE(int id = -1) : id(id) {}

    void add_adjacent(int other_id, const EDGE_DATA &data) {
      adjacent.push_back(EDGE_ENTRY(other_id, data));
    }
  };

  std::vector<NODE> nodes;
  size_t n_of_edges = 0;

 public:
  class NODE_PROXY {
    friend class EDGE_PROXY;

   public:
    const int id;
    NODE_DATA &data;

    // private:
    static NODE_DATA null_data;

    bool is_valid() const { return &data != &null_data; }

    NODE_PROXY(graph &g, int id)
        : id(id), data(id >= 0 ? g.nodes[id].data : null_data) {}
  };

  class EDGE_PROXY {
    friend class graph;

   public:
    const NODE_PROXY from, to;
    EDGE_DATA &edge_data;

    // private:
    static EDGE_DATA null_data;

    EDGE_PROXY(graph &g, int from, typename NODE::EDGE_ENTRY &entry)
        : from(g, from), to(g, entry.other_id), edge_data(entry.data) {}

    EDGE_PROXY(graph &g, int to)
        : from(g, -1), to(g, to), edge_data(null_data) {}
  };

  graph() {}

  void resize(size_t size) {
    size_t start = nodes.size();
    nodes.resize(size);
    for (size_t i = start; i < size; i++) nodes[i].id = (int)i;
  }

  graph reverse() {
    graph res;
    res.resize(size());
    for (auto &n : nodes)
      for (auto &e : n.adjacent) res.add_edge(e.other_id, n.id, e.data);
    return res;
  }

  size_t size() const { return nodes.size(); }
  size_t n_edges() const { return n_of_edges; }

  void clear() { nodes.clear(); }

  using weight_t = int;
  void add_edge(int from, int to, const EDGE_DATA &data = {}) {
    auto it = std::find_if(nodes[from].adjacent.begin(), nodes[from].adjacent.end(),
                           [&](const auto &x) { return x.other_id == to; });
    if (it != nodes[from].adjacent.end()) {
      it->data = data;
    } else {
      nodes[from].add_adjacent(to, data);
      n_of_edges++;
    }
  }
  std::enable_if_t<std::is_convertible_v<EDGE_DATA, weight_t>, weight_t>
    get_edge(int from, int to) {
    auto it = std::find_if(nodes[from].adjacent.begin(), nodes[from].adjacent.end(),
                           [&](const auto &x) { return x.other_id == to; });
    if (it != nodes[from].adjacent.end()) {
      return it->data;
    } else {
      return infinity<weight_t>;
    }
  }

  std::enable_if_t<std::is_convertible_v<EDGE_DATA, weight_t>, weight_t> mst() {
    // Kruskal
    struct edge_entry {
      int u, w;
      weight_t weight;

      bool operator<(const edge_entry &other) const {
        return weight < other.weight;
      }
    };
    std::vector<edge_entry> edges;
    for (auto &u : nodes)
      for (auto &uv : u.adjacent)
        if (u.id < uv.other_id)
          edges.push_back({u.id, uv.other_id, uv.data});
    std::sort(edges.begin(), edges.end());

    disjoint_set comps(nodes.size());
    weight_t res = 0;
    int m = 0;
    for (auto uw : edges) {
      if (comps.find(uw.u) != comps.find(uw.w)) {
        // Edge is safe, add to mst
        comps.join(uw.u, uw.w);
        res += uw.weight;
        m++;
        if (m == (int)nodes.size() - 1) break;
      }
    }

    return res;
  }

  const NODE_DATA &operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA &operator[](size_t ind) { return nodes[ind].data; }
};

using EDGE_DATA_T = int;
using graph_t = graph<empty, EDGE_DATA_T>;

graph_t read_graph(std::istream &in) {
  int n, m;
  in >> n >> m;
  graph_t g;
  g.resize(n);

  for (int i = 0; i < m; i++) {
    int from, to, w;
    in >> from >> to >> w;
    --from, --to;
    if (from != to && g.get_edge(from, to) > w) {
      g.add_edge(from, to, w);
      g.add_edge(to, from, w);
    }
  }
  return std::move(g);
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  graph_t g = read_graph(std::cin);
  std::cout << g.mst() << std::endl;
  return 0;
}
