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
    int id;
    NODE_DATA data;
    std::vector<std::pair<int, EDGE_DATA>> adjacent;

    NODE(int id = -1) : id(id) {}

    void add_adjacent(int other_id, const EDGE_DATA &data) {
      adjacent.push_back({other_id, data});
    }
  };

 public:
  enum class color {
    white, gray, black
  };
  std::vector<color> cols;

 private:
  std::vector<NODE> nodes;

 public:
  class NODE_PROXY {
    friend class EDGE_PROXY;

   public:
    const int id;
    NODE_DATA &data;
    const color &col;

    // private:
    static NODE_DATA null_data;

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

    EDGE_PROXY(graph &g, int from, int to, EDGE_DATA &edge_data)
        : from(g, from), to(g, to), edge_data(edge_data) {}

    EDGE_PROXY(graph &g, int to) : from(g, -1), to(g, to), edge_data(null_data) {}
  };


  void exec_dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
                const std::function<void(NODE_PROXY self,
                  const std::vector<EDGE_PROXY> &to_children)> &after,
                EDGE_PROXY from_parent) {
    int node_id = from_parent.to.id;
    if (before(from_parent) || cols[node_id] != color::white) return;
    cols[node_id] = color::gray;
    std::vector<EDGE_PROXY> to_children;
    for (auto next : nodes[node_id].adjacent) {
      EDGE_PROXY to_child = EDGE_PROXY(*this, node_id, next.first, next.second);
      exec_dfs(before, after, to_child);
      to_children.push_back(to_child);
    }
    after(from_parent.to, to_children);
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
      for (auto &e : n.adjacent) res.add_edge(e.first, n.id, e.second);
    return res;
  }

  size_t size() const { return nodes.size(); }

  void clear() {
    nodes.clear();
    cols.clear();
  }

  void add_edge(int from, int to, const EDGE_DATA &data = {}) {
    nodes[from].add_adjacent(to, data);
  }

  void dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
           const std::function<void(NODE_PROXY self,
             const std::vector<EDGE_PROXY> &to_children)> &after,
           int start_at) {
    std::fill(cols.begin(), cols.end(), color::white);
    exec_dfs(before, after, EDGE_PROXY(*this, start_at));
  }

  void dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
           const std::function<void(NODE_PROXY self,
             const std::vector<EDGE_PROXY> &to_children)> &after,
           const std::function<void()> &again_callback) {
    std::fill(cols.begin(), cols.end(), color::white);
    for (int i = 0; i < (int)nodes.size(); i++)
      if (cols[i] == color::white) {
        again_callback();
        exec_dfs(before, after, EDGE_PROXY(*this, i));
      }
  }

  const NODE_DATA & operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA & operator[](size_t ind) { return nodes[ind].data; }
};

template<class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template<class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

using graph_t = graph<int, empty>;

graph_t read_graph(std::istream &i) {
  graph_t g;

  int N, M;
  i >> N >> M;

  g.resize(N);
  for (int j = 0; j < M; j++) {
    int adj1, adj2;
    i >> adj1 >> adj2;
    g.add_edge(adj1 - 1, adj2 - 1);
  }
  return g;
}

constexpr int n_inf = std::numeric_limits<int>::min();

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  graph_t g = read_graph(std::cin);

  int max_path = 0;

  g.dfs(
    [&](graph_t::EDGE_PROXY) {
      return false;
    },
    [&](graph_t::NODE_PROXY self, const std::vector<graph_t::EDGE_PROXY> &ch) {
      if (ch.empty()) {
        self.data = 0;
      } else {
        self.data = std::accumulate(ch.begin(), ch.end(), n_inf,
                                [&](auto x, auto y) {
                                  return std::max(x, y.to.data);
                                }) + 1;
        max_path = std::max(max_path, self.data);
      }
    },
    [&]() {
    });

  std::cout << (max_path == (int)g.size() - 1 ? "YES" : "NO") << std::endl;
  return 0;
}
