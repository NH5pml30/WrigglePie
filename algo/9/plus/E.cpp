/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <iostream>
#include <limits>
#include <queue>
#include <tuple>
#include <vector>

template <typename T = int>
constexpr T infinity = std::numeric_limits<T>::max();

template <typename T = int>
constexpr T neg_infinity = std::numeric_limits<T>::min();

template<typename weight_t>
struct number {
  static constexpr weight_t inf = infinity<weight_t>, neg_inf = neg_infinity<weight_t>;
  weight_t data;

  number(weight_t d) : data(d) {}

  number operator+(number other) const {
    if (other.data > 0 && data > inf - other.data) return inf;
    if (other.data < 0 && data < neg_inf - other.data) return neg_inf;
    return data + other.data;
  }

  bool operator<(number other) const {
    return data < other.data;
  }
  bool operator>(number other) const {
    return data > other.data;
  }
  bool is_inf() const {
    return data == inf;
  }
  bool is_neg_inf() const {
    return data == neg_inf;
  }
  bool is_nan() const {
    return is_inf() || is_neg_inf();
  }
  bool operator==(number other) const {
    return is_nan() || other.is_nan() ? false : data == other.data;
  }
  bool operator!=(number other) const {
    return is_nan() || other.is_nan() ? false : data != other.data;
  }
};

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

 private:
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
  std::enable_if_t<std::is_same_v<EDGE_DATA, int64_t>, int64_t>
    get_edge(int from, int to) {
    auto it = std::find_if(nodes[from].adjacent.begin(), nodes[from].adjacent.end(),
                           [&](const auto &x) { return x.other_id == to; });
    if (it == nodes[from].adjacent.end()) return infinity<int64_t>;
    return it->data;
  }

  using number = number<int64_t>;
  std::enable_if_t<std::is_same_v<EDGE_DATA, int64_t>, std::vector<number>>
  bellman_ford(int start) {
    std::vector<number> dist(nodes.size(), infinity<int64_t>);
    dist[start] = 0;

    for (int k = 0; k < (int)nodes.size() - 1; k++)
      for (auto &u : nodes)
        if (!dist[u.id].is_inf())
          for (auto &uv : u.adjacent)
            dist[uv.other_id] = std::min(dist[uv.other_id], dist[u.id] + uv.data);
    std::vector<bool> no_short_path(nodes.size());
    for (auto &u : nodes) {
      if (!dist[u.id].is_inf()) {
        for (auto &uv : u.adjacent) {
          if (dist[uv.other_id] > dist[u.id] + uv.data) {
            // Found an unrelaxed vertex from negative cycle,
            // mark all reachable vertices 'no shortest path'
            auto dfs = [&](int start, auto &dfs) -> void {
              no_short_path[start] = true;
              auto &u = nodes[start];
              for (auto &uv : u.adjacent)
                if (!no_short_path[uv.other_id]) dfs(uv.other_id, dfs);
            };
            dfs(uv.other_id, dfs);
          }
        }
      }
    }
    for (int i = 0; i < (int)nodes.size(); i++)
      if (no_short_path[i]) dist[i] = neg_infinity<int64_t>;

    return dist;
  }

  const NODE_DATA &operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA &operator[](size_t ind) { return nodes[ind].data; }
};

template <class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template <class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

struct NODE_DATA_T {};
using EDGE_DATA_T = int64_t;

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

std::pair<graph_t, int> read_graph(std::istream &in) {
  int n, m, s;
  in >> n >> m >> s;
  --s;

  graph_t g;
  g.resize(n);

  for (int i = 0; i < m; i++) {
    int from, to;
    int64_t w;
    in >> from >> to >> w;
    --from, --to;
    if (g.get_edge(from, to) > w)
      g.add_edge(from, to, w);
  }
  return {std::move(g), s};
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto[g, s] = read_graph(std::cin);
  std::vector<number<int64_t>> dist = g.bellman_ford(s);
  for (int i = 0; i < (int)dist.size(); i++, std::cout << std::endl)
    if (dist[i].is_inf())
      std::cout << "*";
    else if (dist[i].is_neg_inf())
      std::cout << "-";
    else
      std::cout << dist[i].data;
  std::cout << std::endl;
  return 0;
}
