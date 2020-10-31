/* Nikolai Kholiavin, M3238 */
#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <cstring>
#include <vector>
#include <tuple>
#include <optional>
#include <numeric>
#include <algorithm>
#include <iterator>
#include <string>
#include <cstdlib>

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
  void increment() {}
  bool equals(const ChildT &other) const {
    return cur == static_cast<const counting_iterator &>(other).cur;
  }
  ChildT &operator++() {
    static_cast<ChildT *>(this)->increment();
    cur++;
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
  bool operator==(const ChildT &other) const {
    return static_cast<const ChildT *>(this)->equals(other);
  }
  bool operator!=(const ChildT &other) const {
    return !operator==(other);
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
  T invoke() const {
    return T();
  }
  auto operator*() const {
    return static_cast<const ChildT *>(this)->invoke();
  }
};

template<typename T>
struct supplier_iterator_default : supplier_iterator<supplier_iterator_default<T>, T> {
  using supplier_iterator<supplier_iterator_default, T>::supplier_iterator;
};

using empty_supplier = supplier_iterator_default<empty>;

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

    void add_adjacent(int other_id, EDGE_DATA data) {
      adjacent.emplace_back(other_id, std::move(data));
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

    NODE_DATA invoke() const {
      return base_t::enclosing->nodes[base_t::cur].data;
    }
  };

  template<class GraphChildT>
  struct node_iterator_default : node_iterator<node_iterator_default<GraphChildT>, GraphChildT> {
    using node_iterator<node_iterator_default, GraphChildT>::node_iterator;
  };

  static constexpr bool IS_EMPTY_EDGE = std::is_same_v<EDGE_DATA, empty>;
  using edge_descriptor_t =
      std::conditional_t<IS_EMPTY_EDGE, std::pair<int, int>, std::tuple<int, int, EDGE_DATA>>;
  static edge_descriptor_t create_edge_descriptor(int from, int to, EDGE_DATA data = {}) {
    if constexpr (IS_EMPTY_EDGE)
      return edge_descriptor_t(from, to);
    else
      return edge_descriptor_t(from, to, std::move(data));
  }
  static edge_descriptor_t create_edge_descriptor(int from, const typename NODE::EDGE_ENTRY &edge) {
    return create_edge_descriptor(from, edge.other_id, edge.data);
  }

  template<class ChildT, class GraphChildT>
  struct edge_iterator
      : base_graph_iterator<ChildT, GraphChildT, edge_descriptor_t> {
    using base_t = base_graph_iterator<ChildT, GraphChildT, edge_descriptor_t>;
    int from, off = -1;

    edge_iterator(GraphChildT *enclosing, int from) : base_t(enclosing), from(from) {
      increment();
    }
    edge_descriptor_t invoke() const {
      return create_edge_descriptor(from, base_t::enclosing->nodes[from].adjacent[off].other_id);
    }
    void increment() {
      off++;
      while (from < (int)base_t::enclosing->nodes.size() &&
             off >= (int)base_t::enclosing->nodes[from].adjacent.size()) {
        off = 0;
        from++;
      }
    }
    bool equals(const edge_iterator &other) const {
      return from == other.from && off == other.off;
    }
  };

  template<class GraphChildT>
  struct edge_iterator_default : edge_iterator<edge_iterator_default<GraphChildT>, GraphChildT> {
    using edge_iterator<edge_iterator_default, GraphChildT>::edge_iterator;
  };

  std::vector<NODE> nodes;
  int n_of_edges_;

  template<class EdgeIter>
  graph(int n, EdgeIter edge_begin, EdgeIter edge_end)
      : graph(n, edge_begin, edge_end, supplier_iterator_default<NODE_DATA>(0),
              supplier_iterator_default<NODE_DATA>(n)) {}

  template<class EdgeIter, class NodeIter>
  graph(int n, EdgeIter edge_begin, EdgeIter edge_end, NodeIter node_begin, NodeIter node_end) {
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
        nodes[from].add_adjacent(to, std::move(data));
      }
      n_of_edges_++;
    }
  }

  graph reverse() const {
    struct reverse_edge_iterator : edge_iterator<reverse_edge_iterator, const graph> {
      using base_t = edge_iterator<reverse_edge_iterator, const graph>;

      using base_t::edge_iterator;
      edge_descriptor_t invoke() const {
        auto &to = base_t::enclosing->nodes[base_t::from].adjacent[base_t::off];
        return create_edge_descriptor(to.other_id, base_t::from, to.data);
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

struct euler_flag_base {
  mutable bool used = false;

  void set_used(bool new_used) const {
    used = new_used;
  }
  bool get_used() const {
    return used;
  }
};

template<class NODE_DATA, class EDGE_DATA, class EdgeCallback>
std::enable_if_t<std::is_base_of_v<euler_flag_base, EDGE_DATA>>
    euler_(const graph<NODE_DATA, EDGE_DATA> &g, EdgeCallback &callback, int start) {
  std::vector<int> stack = {start};
  stack.reserve(g.size());

  while (!stack.empty()) {
    int v = stack.back();
    bool ended = true;
    for (auto &next : g.nodes[v].adjacent)
      if (!next.data.get_used()) {
        next.data.set_used(true);
        stack.push_back(next.other_id);
        ended = false;
        break;
      }
    if (ended) {
      stack.pop_back();
      callback(v);
    }
  }
}

template<class NODE_DATA, class EDGE_DATA, class EdgeCallback>
std::enable_if_t<std::is_base_of_v<euler_flag_base, EDGE_DATA>>
    euler(const graph<NODE_DATA, EDGE_DATA> &g, EdgeCallback callback) {
  int start = 0;
  for (int v = 0; v < g.size(); v++) {
    for (auto &next : g.nodes[v].adjacent)
      next.data.set_used(false);
    if (g.nodes[v].adjacent.size() % 2 == 1)
      start = v;
  }

  euler_(g, callback, start);
}

using NODE_DATA_T = empty;
using EDGE_DATA_T = euler_flag_base;

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

int main() {
  // n(edges) = d^k
  // n(edges) - 1 ~ 1'000'000
  // d^k ~ 1'000'000
  // => fits into int
  int d, k;
  std::cin >> d >> k;

  if (k == 1) {
    for (int i = 0; i < d; i++)
      std::cout << i;
    std::cout << std::endl;
    return 0;
  }

  int n = 1;
  for (int i = 0; i < k - 1; i++)
    n *= d;

  auto code2string = [&](int code) {
    std::string res;
    res.resize(k);
    _itoa(code, res.data(), d);
    res.resize(strlen(res.data()));
    return res;
  };

  struct edge_supplier : supplier_iterator<edge_supplier, graph_t::edge_descriptor_t> {
    const int d, n;
    int cur, curd, d_counter = 0;

    edge_supplier(const int d, int from, const int n)
        : d(d), n(n), cur(from), curd(cur * d) {
    }
    graph_t::edge_descriptor_t invoke() const {
      return graph_t::create_edge_descriptor(cur, curd + d_counter);
    }
    void increment() {
      d_counter++;
      if (d_counter == d) {
        cur++;
        curd += d;
        if (curd >= n)
          curd -= n;
        d_counter = 0;
      }
    }
    bool equals(const edge_supplier &other) const {
      return cur == other.cur && d_counter == other.d_counter;
    }
  };

  graph_t g(n, edge_supplier(d, 0, n), edge_supplier(d, n, n));
  euler(g, [&, is_first = true](int v) mutable {
    if (is_first) {
      std::string str = code2string(v);
      for (int i = 0; i < k - 1 - (int)str.length(); i++)
        std::cout << 0;
      std::cout << str;
      is_first = false;
    } else {
      std::cout << v / (n / d);
    }
  });
  std::cout << std::endl;
  return 0;
}
