/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <iostream>
#include <limits>
#include <queue>
#include <vector>
#include <tuple>

template<typename T = int>
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

 public:
  std::vector<uint64_t> dist;

 private:
  std::vector<NODE> nodes;
  size_t n_of_edges = 0;

 public:
  class NODE_PROXY {
    friend class EDGE_PROXY;

   public:
    const int id;
    NODE_DATA &data;
    const uint64_t &d;

    // private:
    static NODE_DATA null_data;

    bool is_valid() const { return &data != &null_data; }

    NODE_PROXY(graph &g, int id)
        : id(id),
          data(id >= 0 ? g.nodes[id].data : null_data),
          d(id >= 0 ? g.dist[id] : static_cast<const uint64_t &>(infinity<uint64_t>)) {}
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
    dist.resize(size);
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

  void clear() {
    nodes.clear();
    dist.clear();
  }

  void add_edge(int from, int to, const EDGE_DATA &data = {}) {
    nodes[from].add_adjacent(to, data);
    n_of_edges++;
  }
  bool exists_edge(int from, int to) {
    return std::any_of(nodes[from].adjacent,
                       [&](const auto &x) { return x.first == to; });
  }

  std::enable_if_t<std::is_same_v<EDGE_DATA, int>, uint64_t> naive_dijkstra(int start, int finish) {
    std::fill(dist.begin(), dist.end(), infinity<uint64_t>);
    dist[start] = 0;
    std::vector<bool> was(nodes.size());

    for (int i = 0; i < (int)nodes.size(); i++) {
      int v = -1;
      for (auto &node : nodes)
        if (!was[node.id] && (v == -1 || dist[node.id] < dist[v])) v = node.id;
      was[v] = true;
      if (v == finish || dist[v] == infinity<uint64_t>) break;
      // relax
      for (auto &next : nodes[v].adjacent)
        dist[next.other_id] =
            std::min(dist[next.other_id], dist[v] + next.data);
    }
    return dist[finish];
  }

  const NODE_DATA &operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA &operator[](size_t ind) { return nodes[ind].data; }
};

template <class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template <class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

struct NODE_DATA_T {};
using EDGE_DATA_T = int;

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

std::tuple<graph_t, int, int> read_graph(std::istream &in) {
  int n, s, f;
  in >> n >> s >> f;
  --s, --f;

  graph_t g;
  g.resize(n);

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++) {
      int w;
      in >> w;
      if (w >= 0 && i != j)
        g.add_edge(i, j, w);
    }
  return {g, s, f};
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto[g, s, f] = read_graph(std::cin);

  std::cout << (int64_t)g.naive_dijkstra(s, f) << std::endl;
  return 0;
}
