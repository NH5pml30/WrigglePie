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

  template<class EdgeIter>
  struct edge_iter_wrapper {
    using tuple_t = std::remove_reference_t<decltype(*std::declval<EdgeIter>())>;
    EdgeIter data;
    int n1;
    mutable tuple_t cached;
    bool is_first = true;

    edge_iter_wrapper(EdgeIter data, int n1) : data(std::move(data)), n1(n1) {}
    edge_iter_wrapper & operator++() {
      if (!is_first)
        ++data;
      is_first ^= 1;
      return *this;
    }
    edge_iter_wrapper operator++(int) {
      auto res = edge_iter_wrapper(*this);
      operator++();
      return res;
    }
    bool operator==(const edge_iter_wrapper &other) const {
      return data == other.data && is_first == other.is_first;
    }
    bool operator!=(const edge_iter_wrapper &other) const {
      return data != other.data || is_first != other.is_first;
    }
    auto operator*() const {
      if (is_first) {
        cached = *data;
        std::get<1>(cached) += n1;
        auto res = cached;
        std::swap(std::get<0>(cached), std::get<1>(cached));
        return res;
      } else {
        return cached;
      }
    }
  };

  int n1, n2;

 public:
  template<class EdgeIter, class NodeIter = empty_supplier>
  bipartite_graph(int n1, int n2, EdgeIter edge_begin, EdgeIter edge_end,
                  NodeIter node_begin = empty_supplier(0), NodeIter node_end = empty_supplier(0))
      : base(n1 + n2, edge_iter_wrapper<EdgeIter>(edge_begin, n1),
             edge_iter_wrapper<EdgeIter>(edge_end, n1), node_begin, node_end),
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
};

using NODE_DATA_T = int;
using EDGE_DATA_T = empty;

using graph_t = bipartite_graph<NODE_DATA_T, EDGE_DATA_T>;

graph_t read_graph(std::istream &in) {
  int n;
  in >> n;

  std::vector<std::tuple<int, int>> weights(n);
  for (int i = 0; i < n; i++)
  {
    in >> std::get<0>(weights[i]);
    std::get<1>(weights[i]) = i;
  }
  std::sort(weights.begin(), weights.end(), std::greater<>());
  std::vector<int> reverse_map(n), map(n);
  for (int i = 0; i < n; i++)
  {
    reverse_map[std::get<1>(weights[i])] = i;
    map[i] = std::get<1>(weights[i]);
  }

  std::vector<std::pair<int, int>> edges;
  for (int i = 0; i < n; i++)
  {
    int m;
    in >> m;
    edges.reserve(edges.size() + m);
    for (int j = 0; j < m; j++)
    {
      int other;
      in >> other;
      edges.push_back({reverse_map[i], other - 1});
    }
  }
  map.resize(2 * n);

  return graph_t(n, n, edges.begin(), edges.end(), map.begin(), map.end());
}

#include <fstream>

int main() {
  auto in = std::ifstream("matching.in");
  auto out = std::ofstream("matching.out");

  graph_t g = read_graph(in);
  std::vector<int> l2r(g.size() / 2, -1);
  g.max_matching([&](int l, int r) { l2r[g[l]] = r; });
  for (int i = 0; i < (int)g.size() / 2; i++)
    out << l2r[i] + 1 << ' ';
  out << std::endl;
  return 0;
}
