/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <optional>
#include <numeric>
#include <algorithm>
#include <functional>
#include <cassert>
#include <iterator>
#include <queue>

struct empty {};

template<class ChildT>
struct counting_iterator {
  using iterator_category = std::forward_iterator_tag;
  using value_type        = int;
  using difference_type   = int;
  using pointer           = int *;
  using reference         = int &;

  int cur = 0;

  counting_iterator(int begin = 0) : cur(begin) {}
  virtual void increment() {}
  ChildT &operator++() {
    cur++;
    increment();
    return static_cast<ChildT &>(*this);
  }
  ChildT operator++(int) {
    auto res = ChildT(static_cast<const ChildT &>(*this));
    operator++();
    return res;
  }
  int operator*() const {
    return cur;
  }
  bool operator==(const counting_iterator &other) const {
    return cur == other.cur;
  }
  bool operator!=(const counting_iterator &other) const {
    return cur != other.cur;
  }
  void reset(int new_cur) {
    cur = new_cur;
  }
};

struct counting_iterator_t : counting_iterator<counting_iterator_t> {
  using base_t = counting_iterator<counting_iterator_t>;
  using base_t::counting_iterator;
};

template<class ChildT, class T>
struct supplier_iterator : counting_iterator<ChildT> {
  using base_t = counting_iterator<ChildT>;
  using value_type = std::remove_reference_t<T>;
  using pointer    = value_type *;
  using reference  = value_type &;

  supplier_iterator(int begin = 0) : base_t(begin) {}
  virtual value_type invoke() const {
    return value_type();
  }
  auto operator*() const {
    return invoke();
  }
};

struct empty_supplier : supplier_iterator<empty_supplier, empty> {
  using supplier_iterator<empty_supplier, empty>::supplier_iterator;
};

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

template<class InputIt, class OutputIt, class ToNumber>
void integer_set_complement(int n, InputIt in_begin, InputIt in_end, OutputIt out_begin,
                            ToNumber func) {
  using number_t = std::decay_t<decltype(func(*in_begin))>;
  static_assert(std::is_integral_v<number_t>);
  std::vector<number_t> initial_vals;
  std::transform(in_begin, in_end, std::back_inserter(initial_vals), func);
  std::sort(initial_vals.begin(), initial_vals.end());
  std::set_difference(counting_iterator_t(), counting_iterator_t(n), initial_vals.begin(),
                      initial_vals.end(), out_begin);
}

template<class NODE_DATA = empty, class EDGE_DATA = empty>
class graph {
 public:
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

  template<class ChildT, class GraphChildT, class T>
  struct base_graph_iterator : supplier_iterator<ChildT, T> {
    using base_t = supplier_iterator<ChildT, T>;
    GraphChildT *enclosing;

    base_graph_iterator(GraphChildT *enclosing, int begin = 0)
        : base_t(begin), enclosing(enclosing) {}
  };

  template<class ChildT, class GraphChildT>
  struct node_iterator : base_graph_iterator<ChildT, GraphChildT, NODE_DATA> {
    using base_t = base_graph_iterator<ChildT, GraphChildT, NODE_DATA>;
    using base_t::base_graph_iterator;

    NODE_DATA invoke() const override {
      return base_t::enclosing->nodes[base_t::cur].data;
    }
  };

  template<class GraphChildT>
  struct node_iterator_default : node_iterator<node_iterator_default<GraphChildT>, GraphChildT> {
    using node_iterator<node_iterator_default, GraphChildT>::node_iterator;
  };

  template<class ChildT, class GraphChildT>
  struct edge_iterator
      : base_graph_iterator<ChildT, GraphChildT, std::pair<int, int>> {
    using base_t = base_graph_iterator<ChildT, GraphChildT, std::pair<int, int>>;
    int from, off = -1;

    edge_iterator(GraphChildT *enclosing, int from) : base_t(enclosing), from(from) {
      increment();
    }
    std::pair<int, int> invoke() const override {
      int to = base_t::enclosing->nodes[from].adjacent[off].other_id;
      return {from, to};
    }
    void increment() override {
      off++;
      while (from < (int)base_t::enclosing->nodes.size() &&
             off >= (int)base_t::enclosing->nodes[from].adjacent.size()) {
        off = 0;
        from++;
      }
    }
    bool operator==(const edge_iterator &other) const {
      return from == other.from && off == other.off;
    }
    bool operator!=(const edge_iterator &other) const {
      return !operator==(other);
    }
  };

  template<class GraphChildT>
  struct edge_iterator_default : edge_iterator<edge_iterator_default<GraphChildT>, GraphChildT> {
    using edge_iterator<edge_iterator_default, GraphChildT>::edge_iterator;
  };

  std::vector<NODE> nodes;
  int n_of_edges_;

  template<class EdgeIter, class NodeIter = empty_supplier>
  graph(int n, EdgeIter edge_begin, EdgeIter edge_end, NodeIter node_begin = empty_supplier(0),
        NodeIter node_end = empty_supplier(0)) {
    if constexpr (std::is_same_v<NodeIter, empty_supplier>) {
      node_end = empty_supplier(n);
    }
    nodes.reserve(n);
    int i = 0;
    for (; node_begin != node_end; ++node_begin)
      nodes.push_back(NODE(i++, *node_begin));
    n_of_edges_ = 0;
    for (; edge_begin != edge_end; ++edge_begin) {
      if constexpr (std::is_same_v<EDGE_DATA, empty>) {
        auto[from, to] = *edge_begin;
        nodes[from].add_adjacent(to, {});
      } else {
        auto[from, to, data] = *edge_begin;
        nodes[from].add_adjacent(to, data);
      }
      n_of_edges_++;
    }
  }

  graph reverse() const {
    struct reverse_edge_iterator : edge_iterator<reverse_edge_iterator, const graph> {
      using base_t = edge_iterator<reverse_edge_iterator, const graph>;

      using base_t::edge_iterator;
      std::pair<int, int> invoke() const override {
        int to = base_t::enclosing->nodes[base_t::from].adjacent[base_t::off].other_id;
        return {to, base_t::from};
      }
      bool operator==(const reverse_edge_iterator &other) const {
        return base_t::operator==(other);
      }
      bool operator!=(const reverse_edge_iterator &other) const {
        return !operator==(other);
      }
    };
    return graph(size(), reverse_edge_iterator(this, 0), reverse_edge_iterator(this, size()),
                 node_iterator_default<const graph>(this, 0),
                 node_iterator_default<const graph>(this, size()));
  }

  int size() const {
    return (int)nodes.size();
  }
  int n_of_edges() const {
    return n_of_edges_;
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
 public:
  using base = graph<NODE_DATA, EDGE_DATA>;
  int n1, n2;

  template<class EdgeIter>
  class bi_edge_supplier
      : public supplier_iterator<bi_edge_supplier<EdgeIter>, decltype(*std::declval<EdgeIter>())> {
    using edge_t = std::remove_reference_t<decltype(*std::declval<EdgeIter>())>;
    using base_t = supplier_iterator<bi_edge_supplier, edge_t>;
    int n1;
    EdgeIter data;

   public:
    bi_edge_supplier(int n1, EdgeIter data)
        : n1(n1), data(std::move(data)) {}
    edge_t invoke() const override {
      auto res = *data;
      std::get<1>(res) += n1;
      return res;
    }
    void increment() override {
      ++data;
    }
    bool operator==(const bi_edge_supplier &other) const {
      return data == other.data;
    }
    bool operator!=(const bi_edge_supplier &other) const {
      return !operator==(other);
    }
  };

  template<class EdgeIter, class NodeIter = empty_supplier>
  bipartite_graph(int n1, int n2, EdgeIter edge_begin, EdgeIter edge_end,
                  NodeIter node_begin = empty_supplier(0), NodeIter node_end = empty_supplier(0))
      : base(n1 + n2, bi_edge_supplier<EdgeIter>(n1, edge_begin),
             bi_edge_supplier<EdgeIter>(n1, edge_end), node_begin, node_end),
        n1(n1),
        n2(n2) {}
};

template<class NODE_DATA = empty, class EDGE_DATA = empty, class MatchCallback>
int HopcroftKarp(bipartite_graph<NODE_DATA, EDGE_DATA> &g, MatchCallback &&callback) {
  constexpr int inf = std::numeric_limits<int>::max();

  // add dummy vertex
  int dummy = (int)g.size();

  g.nodes.push_back({dummy});
  for (int i = 0; i < g.n1; i++)
    g.nodes[dummy].add_adjacent(i, {});
  for (int i = 0; i < g.n2; i++)
    g.nodes[g.n1 + i].add_adjacent(dummy, {});

  std::vector<int> match_r2l_store(g.n2 + 1, dummy), match_l2r_store(g.n1 + 1, dummy);
  std::vector<int> dist_store(g.n1 + 1);
  auto get_from_store = [&](int v, std::vector<int> &store) -> int & {
    return v == dummy ? store.back() : store[v];
  };
  auto match_r2l = [&](int v2) -> int & { return get_from_store(v2, match_r2l_store); };
  auto match_l2r = [&](int v1) -> int & { return get_from_store(v1, match_l2r_store); };
  auto dist = [&](int v1) -> int & { return get_from_store(v1, dist_store); };

  auto bfs = [&]() {
    std::queue<int> q;
    for (int i = 0; i < g.n1; i++) {
      if (match_l2r(i) == dummy) {
        // vertex is unmatched, add to first layer
        dist(i) = 0;
        q.push(i);
      } else {
        // vertex is matched, do nothing
        dist(i) = inf;
      }
    }
    dist(dummy) = inf;  // dummy is also not in the first layer

    while (!q.empty()) {
      int l = q.front();
      q.pop();

      if (dist(l) < dist(dummy)) {
        for (auto next : g.nodes[l].adjacent) {
          int r = next.other_id - g.n1;
          if (dist(match_r2l(r)) == inf) {
            // r & r's match are not yet visited (also meaning that (l, r) is not a match)
            // add to next layer
            dist(match_r2l(r)) = dist(l) + 1;
            q.push(match_r2l(r));
          }
        }
      }
    }

    return dist(dummy) != inf;  // <=> found augmenting path
  };

  std::function<bool(int)> dfs = [&](int l) {
    if (l != dummy) {
      for (auto next : g.nodes[l].adjacent) {
        int r = next.other_id - g.n1;
        if (dist(match_r2l(r)) == dist(l) + 1 && dfs(match_r2l(r))) {
          // came to next layer and found augmenting path
          // match vertices, return true
          match_r2l(r) = l;
          match_l2r(l) = r;
          return true;
        }
      }

      // no augmenting path found
      dist(l) = inf;
      return false;
    }

    return true;
  };

  int res = 0;

  while (bfs())
    for (int l = 0; l < g.n1; l++)
      res += (match_l2r(l) == dummy && dfs(l));  // <=> found augmenting path

  // revert graph
  g.nodes.pop_back();
  for (int i = 0; i < g.n2; i++)
    g.nodes[g.n1 + i].adjacent.pop_back();

  // write result
  for (int l = 0; l < g.n1; l++)
    if (match_l2r(l) != dummy)
      callback(l, match_l2r(l));

  return res;
}

template<class NODE_DATA, class EDGE_DATA>
std::vector<bool> halls_violator(bipartite_graph<NODE_DATA, EDGE_DATA> &g) {
  std::vector<int> matches(g.n2, -1);
  HopcroftKarp(g, [&](int l, int r) { matches[r] = l; });

  std::vector<bool> used(g.n1);
  auto dfs = [&](int v, auto dfs) -> bool {
    used[v] = true;
    for (auto &next : g.nodes[v].adjacent) {
      int u = next.other_id - g.n1;
      if (matches[u] == -1 ||
          (!used[matches[u]] && dfs(matches[u], dfs))) {
        matches[u] = v;
        return true;
      }
    }
    return false;
  };

  std::fill(used.begin(), used.end(), false);
  for (int i = 0; i < g.n2; i++)
    if (matches[i] != -1)
      used[matches[i]] = true;

  int not_covered = -1;
  for (int i = 0; i < g.n1; i++)
    if (!used[i]) {
      not_covered = i;
      break;
    }

  std::fill(used.begin(), used.end(), false);
  if (not_covered == -1)
    return used;
  dfs(not_covered, dfs);

  return used;
}

using NODE_DATA_T = empty;
using EDGE_DATA_T = empty;

using graph_t = bipartite_graph<NODE_DATA_T, EDGE_DATA_T>;

graph_t read_graph(std::istream &in) {
  int n1, n2, m;
  if (!(in >> n1 >> n2 >> m)) {
    std::pair<int, int> *it = nullptr;
    return graph_t(0, 0, it, it);
  }

  struct edge_outer_supplier : supplier_iterator<edge_outer_supplier, std::pair<int, int>> {
    mutable struct counting_istream_iterator : supplier_iterator<counting_istream_iterator, int> {
      mutable std::istream_iterator<int> base;

      using sup_base_t = supplier_iterator<counting_istream_iterator, int>;
      mutable bool value_ready = true;

      counting_istream_iterator(std::istream &in, int begin = 0)
          : sup_base_t(begin), base(in) {}
      counting_istream_iterator(int begin = 0) : sup_base_t(begin) {}
      int invoke() const override {
        if (!value_ready) {
          ++base;
          value_ready = true;
        }
        return *base;
      }
      void increment() override {
        value_ready = false;
      }
      using sup_base_t::operator==;
      using sup_base_t::operator!=;
      using sup_base_t::operator*;
      using sup_base_t::operator++;
    } cur;
    supplier_iterator<counting_istream_iterator, int> end;

    int cached_from, cached_to;

    edge_outer_supplier(std::istream &in, int m)
        : cur(in), end(2 * m) {
      if (cur != end)
        read();
    }
    edge_outer_supplier(int m) : cur(2 * m), end(2 * m) {
    }

    void read() {
      cached_to = *cur;
      ++cur;
      cached_from = *cur;
    }
    std::pair<int, int> invoke() const override {
      ++cur;
      return {cached_from - 1, cached_to - 1};
    }
    void increment() override {
      if (cur == end)
        return;

      read();
    }
    bool operator==(const edge_outer_supplier &other) const {
      return cur == other.cur;
    }
  };

  return graph_t(n2, n1, edge_outer_supplier(in, m), edge_outer_supplier(m));
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  while (true) {
    graph_t g = read_graph(std::cin);
    if (g.n1 == 0)
      break;
    int s = 0;
    std::vector<bool> is_in_set = halls_violator(g);
    for (int i = 0; i < (int)is_in_set.size(); i++)
      s += is_in_set[i];
    std::cout << s << '\n';
    for (int i = 0; i < (int)is_in_set.size(); i++)
      if (is_in_set[i])
        std::cout << i + 1 << ' ';
    std::cout << '\n' << '\n';
  }
  std::cout.flush();
  return 0;
}
