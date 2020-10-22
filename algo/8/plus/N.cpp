/* Kholiavin Nikolai, M3138 */
#include <functional>
#include <iostream>
#include <numeric>
#include <vector>
#include <list>
#include <set>
#include <stack>
#include <algorithm>
#include <ios>

struct empty {
};

template<class NODE_DATA = empty, class EDGE_DATA = empty>
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
  enum class color {
    white, gray, black
  };
  std::vector<color> cols;

 private:
  std::vector<NODE> nodes;
  size_t n_of_edges = 0;

 public:
  class NODE_PROXY {
    friend class EDGE_PROXY;

   public:
    const int id;
    NODE_DATA &data;
    const color &col;

    // private:
    static NODE_DATA null_data;

    bool is_valid() const {
      return &data != &null_data;
    }

    NODE_PROXY(graph &g, int id)
        : id(id),
          data(id >= 0 ? g.nodes[id].data : null_data),
          col(id >= 0 ? g.cols[id] : static_cast<const color &>(color())) {}
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

    EDGE_PROXY(graph &g, int to) : from(g, -1), to(g, to), edge_data(null_data) {}
  };


  void exec_dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
                const std::function<void(EDGE_PROXY from_parent,
                  const std::vector<EDGE_PROXY> &to_children)> &after,
                EDGE_PROXY from_parent) {
    int node_id = from_parent.to.id;
    if (before(from_parent) || cols[node_id] != color::white) return;
    cols[node_id] = color::gray;
    std::vector<EDGE_PROXY> to_children;
    for (auto &next : nodes[node_id].adjacent) {
      EDGE_PROXY to_child = EDGE_PROXY(*this, node_id, next);
      exec_dfs(before, after, to_child);
      to_children.push_back(to_child);
    }
    after(from_parent, to_children);
    cols[node_id] = color::black;
  }

  graph() {}

  void resize(size_t size) {
    size_t start = nodes.size();
    nodes.resize(size);
    cols.resize(size);
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
    cols.clear();
  }

  void add_edge(int from, int to, const EDGE_DATA &data = {}) {
    nodes[from].add_adjacent(to, data);
    n_of_edges++;
  }
  bool exists_edge(int from, int to) {
    return std::any_of(nodes[from].adjacent, [&](const auto &x) { return x.first == to; });
  }

  void dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
           const std::function<void(EDGE_PROXY from_parent,
             const std::vector<EDGE_PROXY> &to_children)> &after,
           int start_at) {
    std::fill(cols.begin(), cols.end(), color::white);
    exec_dfs(before, after, EDGE_PROXY(*this, start_at));
  }

  void dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
           const std::function<void(EDGE_PROXY from_parent,
           const std::vector<EDGE_PROXY> &to_children)> &after,
           const std::function<void()> &start_callback,
           const std::function<void()> &end_callback) {
    std::fill(cols.begin(), cols.end(), color::white);
    for (int i = 0; i < (int)nodes.size(); i++)
      if (cols[i] == color::white) {
        start_callback();
        exec_dfs(before, after, EDGE_PROXY(*this, i));
        end_callback();
      }
  }

  bool is_connected() {
    if (size() == 0)
      return true;

    int last_vertex = 0;
    dfs([](EDGE_PROXY) { return false; },
      [&](EDGE_PROXY from_parent, auto) { last_vertex = from_parent.to.id; },
      [](){}, [](){});

    graph rev = reverse();
    rev.dfs([](auto) { return false; }, [](auto, auto) {}, last_vertex);
    return std::find(rev.cols.begin(), rev.cols.end(), color::white) == rev.cols.end();
  }

  const NODE_DATA & operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA & operator[](size_t ind) { return nodes[ind].data; }
};

template<class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template<class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

struct NODE_DATA_T {
  int max_edge;
};
struct EDGE_DATA_T {
};

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n;
  std::cin >> n;

  if (n == 1) {
    std::cout << 0 << std::endl;
    return 0;
  }

  std::vector<std::vector<int>> adj_mat(n);
  for (int i = 0; i < n; i++) {
    adj_mat[i].resize(n, 0);
    for (int j = 0; j < n; j++)
      std::cin >> adj_mat[i][j];
  }

  struct edge {
    int i, j, weight;

    bool operator<(const edge &other) const {
      return weight < other.weight;
    }
  };

  std::vector<edge> edges;
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      if (i != j)
        edges.push_back({i, j, adj_mat[i][j]});
  std::sort(edges.begin(), edges.end());

  int l = n, r = (int)edges.size();
  while (l < r) {
    graph_t g;
    g.resize(n);
    int m = (l + r) / 2;

    for (int i = 0; i < m; i++)
      g.add_edge(edges[i].i, edges[i].j, {});

    if (g.is_connected())
      r = m;
    else
      l = m + 1;
  }

  std::cout << edges[r - 1].weight << std::endl;
  return 0;
}
