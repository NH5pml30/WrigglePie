/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <optional>
#include <numeric>
#include <algorithm>
#include <functional>

struct empty {};

struct noop_t {
  void operator()(...) const {}
} noop;

template<auto val>
struct noop_r_t {
  auto operator()(...) const {
    return val;
  }
};

template<auto val>
noop_r_t<val> noop_r;

template<class NODE_DATA = empty, class EDGE_DATA = empty>
class graph {
 protected:
  struct NODE {
    struct EDGE_ENTRY {
      int other_id;
      EDGE_DATA data;

      EDGE_ENTRY() {}
      EDGE_ENTRY(int other_id, const EDGE_DATA &data) : other_id(other_id), data(data) {}
      EDGE_ENTRY(int other_id, EDGE_DATA &&data) : other_id(other_id), data(std::move(data)) {}

      bool operator<(const EDGE_ENTRY &other) const {
        return data < other.data;
      }
    };
    int id;
    NODE_DATA data;
    std::vector<EDGE_ENTRY> adjacent;

    NODE(int id = -1) : id(id) {}
    NODE(int id, NODE_DATA data) : id(id), data(std::move(data)) {}

    void add_adjacent(int other_id, const EDGE_DATA &data) {
      adjacent.push_back(EDGE_ENTRY(other_id, data));
    }
  };
  std::vector<NODE> nodes;

  struct empty_supplier {
    int pos;

    empty_supplier(int pos) : pos(pos) {}
    empty operator*() const {
      return empty();
    }
    bool operator==(const empty_supplier &other) const {
      return pos == other.pos;
    }
    bool operator!=(const empty_supplier &other) const {
      return !operator==(other);
    }
    empty_supplier & operator++() {
      pos++;
      return *this;
    }
    empty_supplier operator++(int) {
      empty_supplier res(pos);
      operator++();
      return res;
    }
  };

 public:
  enum class color { white, gray, black };
  std::vector<color> cols;

  template<class EdgeIter, class NodeIter = empty_supplier>
  graph(int n, EdgeIter edge_begin, EdgeIter edge_end, NodeIter node_begin = empty_supplier(0),
        NodeIter node_end = empty_supplier(0))
      : cols(n) {
    if constexpr (std::is_same_v<NodeIter, empty_supplier>) {
      node_end = empty_supplier(n);
    }
    nodes.reserve(n);
    int i = 0;
    for (; node_begin != node_end; ++node_begin)
      nodes.push_back(NODE(i++, *node_begin));
    for (; edge_begin != edge_end; ++edge_begin) {
      if constexpr (std::is_same_v<EDGE_DATA, empty>) {
        auto[from, to] = *edge_begin;
        nodes[from].add_adjacent(to, {});
      } else {
        auto[from, to, data] = *edge_begin;
        nodes[from].add_adjacent(to, data);
      }
    }
  }

  graph reverse() const {
    graph res;
    res.resize(size());
    for (auto &n : nodes)
      for (auto &e : n.adjacent) res.add_edge(e.other_id, n.id, e.data);
    return res;
  }

  size_t size() const {
    return nodes.size();
  }

  std::optional<EDGE_DATA> get_edge(int from, int to) const {
    auto it = std::find_if(nodes[from].adjacent.begin(), nodes[from].adjacent.end(),
                           [&](const auto &x) { return x.other_id == to; });
    if (it == nodes[from].adjacent.end())
      return {};
    return it->data;
  }

  const NODE_DATA &operator[](size_t ind) const {
    return nodes[ind].data;
  }
  NODE_DATA &operator[](size_t ind) {
    return nodes[ind].data;
  }
};

template<class NODE_DATA = empty, class EDGE_DATA = empty>
class bipartite_graph : public graph<NODE_DATA, EDGE_DATA> {
  using base = graph<NODE_DATA, EDGE_DATA>;
  using typename base::empty_supplier;
  int n1, n2;

 public:
  template<class EdgeIter, class NodeIter = empty_supplier>
  bipartite_graph(int n1, int n2, EdgeIter edge_begin, EdgeIter edge_end,
                  NodeIter node_begin = empty_supplier(0), NodeIter node_end = empty_supplier(0))
      : base(n1 + n2, edge_begin, edge_end, node_begin, node_end),
        n1(n1),
        n2(n2) {}

  template<class MatchCallback>
  int max_matching(MatchCallback callback) {
    std::vector<int> matches(n2, -1);
    int m = 0;

    std::vector<bool> used(n1);
    auto dfs = [&](int v, auto dfs) -> bool {
      used[v] = true;
      for (auto &next : base::nodes[v].adjacent) {
        int u = next.other_id - n1;
        if (matches[u] == -1 ||
            (!used[matches[u]] && dfs(matches[u], dfs))) {
          matches[u] = v;
          return true;
        }
      }
      return false;
    };

    for (int v = 0; v < n1; v++) {
      std::fill(used.begin(), used.end(), false);
      m += dfs(v, dfs);
    }

    for (int i = 0; i < n2; i++)
      if (matches[i] != -1)
        callback(matches[i], i);
    return m;
  }

  template<class VertexCallback>
  int build_min_vertex_cover(std::vector<int> &max_matching, VertexCallback callback) {
    std::vector<int> r2l_match(n2, -1);
    for (int v = 0; v < n1; v++)
      if (max_matching[v] != -1)
        r2l_match[max_matching[v]] = v;

    std::vector<bool> visited_l(n1), visited_r(n2);
    std::function<void(int)> dfs = [&](int v) {
      visited_l[v] = true;
      for (auto &next : base::nodes[v].adjacent) {
        int u = next.other_id - n1;
        if (max_matching[v] != u && !visited_r[u]) {
          visited_r[u] = true;
          dfs(r2l_match[u]);
        }
      }
    };

    int c = 0;
    for (int v = 0; v < n1; v++)
      if (!visited_l[v] && max_matching[v] == -1)
        dfs(v);

    for (int v = 0; v < n1; v++)
      if (!visited_l[v]) {
        callback(v);
        c++;
      }
    for (int v = 0; v < n2; v++)
      if (visited_r[v]) {
        callback(n1 + v);
        c++;
      }
    return c;
  }
};

using NODE_DATA_T = empty;
using EDGE_DATA_T = empty;

using graph_t = bipartite_graph<NODE_DATA_T, EDGE_DATA_T>;

std::tuple<graph_t, int> read_graph(std::istream &in) {
  int n, m;
  in >> m >> n;

  std::vector<std::pair<int, int>> edges;
  for (int i = 0; i < m; i++) {
    int ki;
    in >> ki;
    for (int j = 0; j < ki; j++) {
      int to;
      in >> to;
      to = m + to - 1;
      edges.push_back({i, to});
    }
  }

  return {graph_t(m, n, edges.begin(), edges.end()), m};
}

int main() {
  auto[g, n1] = read_graph(std::cin);

  std::vector<int> matching(n1);
  for (int i = 0; i < n1; i++) {
    std::cin >> matching[i];
    matching[i]--;
  }

  int n_l = n1;
  std::vector<int> cover_l, cover_r;
  std::cout << g.build_min_vertex_cover(matching, [&](int v) {
    if (v < n_l)
      cover_l.push_back(v);
    else
      cover_r.push_back(v - n_l);
  });
  std::cout << std::endl << cover_l.size() << ' ';
  for (auto el : cover_l)
    std::cout << el + 1 << ' ';
  std::cout << std::endl << cover_r.size() << ' ';
  for (auto el : cover_r)
    std::cout << el + 1 << ' ';
  std::cout << std::endl;

  return 0;
}
