/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <iostream>
#include <limits>
#include <queue>
#include <vector>

constexpr int infinity = std::numeric_limits<int>::max();

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
  std::vector<int> bfs_res;

 private:
  std::vector<NODE> nodes;
  size_t n_of_edges = 0;

 public:
  class NODE_PROXY {
    friend class EDGE_PROXY;

   public:
    const int id;
    NODE_DATA &data;
    const int &d;

    // private:
    static NODE_DATA null_data;

    bool is_valid() const { return &data != &null_data; }

    NODE_PROXY(graph &g, int id)
        : id(id),
          data(id >= 0 ? g.nodes[id].data : null_data),
          d(id >= 0 ? g.bfs_res[id] : static_cast<const int &>(infinity)) {}
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
    bfs_res.resize(size);
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
    bfs_res.clear();
  }

  void add_edge(int from, int to, const EDGE_DATA &data = {}) {
    nodes[from].add_adjacent(to, data);
    n_of_edges++;
  }
  bool exists_edge(int from, int to) {
    return std::any_of(nodes[from].adjacent,
                       [&](const auto &x) { return x.first == to; });
  }

  template <class OnUpdateFunc>
  void bfs(OnUpdateFunc on_update, int start_at) {
    std::fill(bfs_res.begin(), bfs_res.end(), infinity);
    bfs_res[start_at] = 0;

    std::queue<int> q;
    q.push(start_at);
    while (!q.empty()) {
      int v = q.front();
      q.pop();
      for (auto &next : nodes[v].adjacent) {
        if (bfs_res[next.other_id] >= bfs_res[v] + 1) {
          on_update(next, bfs_res[next.other_id], bfs_res[v] + 1);
          bfs_res[next.other_id] = bfs_res[v] + 1;
          q.push(next.other_id);
        }
      }
    }
  }

  const NODE_DATA &operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA &operator[](size_t ind) { return nodes[ind].data; }
};

template <class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template <class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

struct NODE_DATA_T {};
struct EDGE_DATA_T {};

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

graph_t read_graph(std::istream &in) {
  int n, m;
  in >> n >> m;

  graph_t g;
  g.resize(n);

  for (int i = 0; i < m; i++) {
    int from, to;
    in >> from >> to;
    --from, --to;
    g.add_edge(from, to);
    g.add_edge(to, from);
  }
  return g;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  graph_t g = read_graph(std::cin);
  g.bfs([](auto, auto, auto) {}, 0);

  for (int i = 0; i < (int)g.size(); i++)
    std::cout << g.bfs_res[i] << ' ';
  std::cout << std::endl;
  return 0;
}
