/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <optional>
#include <numeric>
#include <algorithm>
#include <functional>
#include <cassert>
#include <unordered_set>
#include <iterator>
#include <set>

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
  using value_type = T;
  using pointer    = T *;
  using reference  = T &;

  supplier_iterator(int begin = 0) : base_t(begin) {}
  virtual T invoke() const {
    return T();
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
    using edge_t = decltype(*std::declval<EdgeIter>());
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

template<class NODE_DATA, class EDGE_DATA>
std::vector<int> condense(const graph<NODE_DATA, EDGE_DATA> &g) {
  graph rev = g.reverse();

  std::vector<int> topo_sorted;
  topo_sorted.reserve(g.size());

  std::vector<bool> used(g.size(), false);
  std::function<void(int)> dfs1 = [&](int v) {
    used[v] = true;
    for (auto &next : g.nodes[v].adjacent)
      if (!used[next.other_id])
        dfs1(next.other_id);
    topo_sorted.push_back(v);
  };

  for (int i = 0; i < (int)g.size(); i++)
    if (!used[i])
      dfs1(i);

  std::vector<int> results(g.size(), -1);
  int comp_id = 0;
  std::fill(used.begin(), used.end(), false);

  std::function<void(int)> dfs2 = [&](int v) {
    used[v] = true;
    results[v] = comp_id;
    for (auto &next : rev.nodes[v].adjacent)
      if (!used[next.other_id] && results[next.other_id] == -1)
        dfs2(next.other_id);
  };

  for (int i = (int)topo_sorted.size() - 1; i >= 0; i--)
    if (int node = topo_sorted[i]; results[node] == -1) {
      dfs2(node);
      comp_id++;
    }

  return results;
}

template<class NODE_DATA, class EDGE_DATA>
graph<NODE_DATA, EDGE_DATA> orient_edges(const bipartite_graph<NODE_DATA, EDGE_DATA> &g,
                                         const std::vector<int> &matching) {
  using GraphChildT = const bipartite_graph<NODE_DATA, EDGE_DATA>;
  struct orient_edges_iterator
      : graph<NODE_DATA, EDGE_DATA>::template edge_iterator<orient_edges_iterator, GraphChildT> {
    using base_t =
        typename graph<NODE_DATA, EDGE_DATA>::template edge_iterator<orient_edges_iterator,
                                                                     GraphChildT>;
    const std::vector<int> &matching;

    orient_edges_iterator(GraphChildT *enclosing, int from, const std::vector<int> &matching)
        : base_t(enclosing, from), matching(matching) {}
    std::pair<int, int> invoke() const override {
      int to = base_t::enclosing->nodes[base_t::from].adjacent[base_t::off].other_id;
      if (to - base_t::enclosing->n1 == matching[base_t::from])
        return {to, base_t::from};
      else
        return {base_t::from, to};
    }
    bool operator==(const orient_edges_iterator &other) const {
      return base_t::operator==(other);
    }
    bool operator!=(const orient_edges_iterator &other) const {
      return !operator==(other);
    }
  };

  return graph<NODE_DATA, EDGE_DATA>(g.n1 + g.n2,
      orient_edges_iterator(&g, 0, matching),
      orient_edges_iterator(&g, g.n1 + g.n2, matching),
      typename GraphChildT::template node_iterator_default<GraphChildT>(&g),
      typename GraphChildT::template node_iterator_default<GraphChildT>(&g, g.n1 + g.n2));
}

template<class NODE_DATA, class EDGE_DATA, class EdgeCallback>
void find_edges_in_some_max_matching(bipartite_graph<NODE_DATA, EDGE_DATA> &g,
                                     const std::vector<int> &max_matching, EdgeCallback callback) {
  auto v2c = condense(orient_edges(g, max_matching));
  for (int i = 0; i < g.n1; i++) {
    callback(i, max_matching[i]);
    for (auto &next : g.nodes[i].adjacent)
      if (v2c[i] == v2c[next.other_id] && next.other_id - g.n1 != max_matching[i]) {
        callback(i, next.other_id - g.n1);
      }
  }
}

using NODE_DATA_T = empty;
using EDGE_DATA_T = empty;

using graph_t = bipartite_graph<NODE_DATA_T, EDGE_DATA_T>;

std::pair<graph_t, int> read_graph(std::istream &in) {
  int n;
  in >> n;

  struct edge_outer_supplier : supplier_iterator<edge_outer_supplier, std::pair<int, int>> {
    struct counting_istream_iterator : std::istream_iterator<int>,
                                       supplier_iterator<counting_istream_iterator, int> {
      using sup_base_t = supplier_iterator<counting_istream_iterator, int>;
      using input_base_t = std::istream_iterator<int>;

      counting_istream_iterator(std::istream &in, int begin = 0)
          : input_base_t(in), sup_base_t(begin) {}
      counting_istream_iterator(int begin = 0) : input_base_t(), sup_base_t(begin) {}
      int invoke() const override {
        return input_base_t::operator*();
      }
      void increment() override {
        input_base_t::operator++();
      }
      using sup_base_t::operator==;
      using sup_base_t::operator!=;
      using sup_base_t::operator*;
      using sup_base_t::operator++;
    } cur;
    supplier_iterator<counting_istream_iterator, int> end;
    std::istream_iterator<int> eos;
    int from, from_to;

    edge_outer_supplier(std::istream &in, int from, int from_to)
        : cur(in), end(), from(from), from_to(from_to) {
      read();
    }
    edge_outer_supplier(int from, int from_to) : cur(), end(), from(from), from_to(from_to) {
    }

    void read() {
      if (static_cast<std::istream_iterator<int> &>(cur) != eos)
        end.reset(*cur);
      else
        end.reset(0);
      ++cur;
      cur.reset(0);
    }
    std::pair<int, int> invoke() const override {
      return {from, *cur - 1};
    }
    void increment() override {
      if (cur.counting_iterator::operator==(end))
        return;

      if (cur.cur + 1 != end.cur || from + 1 != from_to) {
        ++cur;
        if (cur == end) {
          ++from;
          read();
        }
      } else {
        ++from;
        cur = counting_istream_iterator();
        end.reset(0);
      }
    }
    bool operator==(const edge_outer_supplier &other) const {
      return from == other.from && cur == other.cur;
    }
  };

  return {graph_t(n, n, edge_outer_supplier(in, 0, n), edge_outer_supplier(n, n)), n};
}

int main() {
  auto[g, n] = read_graph(std::cin);

  std::vector<int> max_matching(n);
  for (int i = 0; i < n; i++) {
    std::cin >> max_matching[i];
    max_matching[i]--;
  }

  std::vector<std::vector<int>> list(n);

  find_edges_in_some_max_matching(
      g, max_matching, [&](int spouse1, int spouse2) { list[spouse1].push_back(spouse2); });

  for (int i = 0; i < n; i++) {
    std::cout << list[i].size();
    for (auto el : list[i])
      std::cout << ' ' << el + 1;
    std::cout << std::endl;
  }

  return 0;
}
