/* Nikolai Kholiavin, M3238 */
#include <algorithm>
#include <cassert>
#include <exception>
#include <functional>
#include <iostream>
#include <iterator>
#include <limits>
#include <numeric>
#include <optional>
#include <queue>
#include <vector>
#include <list>

template <typename T = int>
constexpr T infinity = std::numeric_limits<T>::max();

template <typename T = int>
constexpr T neg_infinity = std::numeric_limits<T>::min();

struct empty {};

template<class ChildT>
struct counting_iterator {
  using iterator_category = std::forward_iterator_tag;
  using value_type = int;
  using difference_type = int;
  using pointer = int *;
  using reference = int &;

  int cur = 0;

  counting_iterator(int begin = 0) : cur(begin) {}
  virtual void increment() {}
  virtual bool equals(const ChildT &other) const {
    return cur == static_cast<const counting_iterator &>(other).cur;
  }
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
    return equals(static_cast<const ChildT &>(other));
  }
  bool operator!=(const counting_iterator &other) const {
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
  using value_type = std::remove_reference_t<T>;
  using pointer = value_type *;
  using reference = value_type &;

  supplier_iterator(int begin = 0) : base_t(begin) {}
  virtual reference invoke() const {
    static value_type val = value_type();
    return val;
  }
  reference operator*() const {
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
    std::list<EDGE_ENTRY> adjacent;

    NODE(int id = -1) : id(id) {}
    NODE(int id, NODE_DATA data) : id(id), data(std::move(data)) {}

    void add_adjacent(int other_id, EDGE_DATA data) {
      adjacent.push_back(EDGE_ENTRY(other_id, std::move(data)));
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
  struct node_iterator : base_graph_iterator<ChildT, GraphChildT, const NODE_DATA> {
    using base_t = base_graph_iterator<ChildT, GraphChildT, const NODE_DATA>;
    using base_t::base_graph_iterator;

    const NODE_DATA &invoke() const override {
      return base_t::enclosing->nodes[base_t::cur].data;
    }
  };

  template<class GraphChildT>
  struct node_iterator_default : node_iterator<node_iterator_default<GraphChildT>, GraphChildT> {
    using node_iterator<node_iterator_default, GraphChildT>::node_iterator;
  };

  template<class ChildT, class GraphChildT>
  struct edge_iterator : base_graph_iterator<ChildT, GraphChildT, const std::pair<int, int>> {
    using base_t = base_graph_iterator<ChildT, GraphChildT, const std::pair<int, int>>;
    std::pair<int, int> desc;
    int off = -1;

    edge_iterator(GraphChildT *enclosing, int from) : base_t(enclosing), desc({from, -1}) {
      increment();
    }
    const std::pair<int, int> &invoke() const override {
      desc.second = base_t::enclosing->nodes[desc.first].adjacent[off].other_id;
      return desc;
    }
    void increment() override {
      off++;
      while (desc.first < (int)base_t::enclosing->nodes.size() &&
             off >= (int)base_t::enclosing->nodes[desc.first].adjacent.size()) {
        off = 0;
        desc.first++;
      }
    }
    bool equals(const edge_iterator &other) const override {
      return desc.first == other.desc.first && off == other.off;
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
        auto &&[from, to, data] = *edge_begin;
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

template<class EdgeT>
struct pair_edge_base {
  mutable EdgeT *paired = nullptr;

  pair_edge_base() = default;
  pair_edge_base(EdgeT *paired) : paired(paired) {
    if (paired != nullptr)
      paired->pair_edge_base::paired = static_cast<EdgeT *>(this);
  }
  pair_edge_base(pair_edge_base &&other) noexcept
      : pair_edge_base(static_cast<const pair_edge_base &>(other)) {}
  pair_edge_base &operator=(pair_edge_base &&other) noexcept {
    return operator=(static_cast<const pair_edge_base &>(other));
  }

  pair_edge_base(const pair_edge_base &other) : paired(other.paired) {
    other.paired = nullptr;
    if (paired != nullptr)
      paired->paired = static_cast<EdgeT *>(this);
  }
  pair_edge_base &operator=(const pair_edge_base &other) {
    paired = other.paired;
    other.paired = nullptr;
    if (paired != nullptr)
      paired->paired = static_cast<EdgeT *>(this);
    return *this;
  }
};

using capacity_t = int;

struct node_data_t {
  int layer;
  mutable capacity_t excess_flow = 0;

  node_data_t(int layer) : layer(layer) {}
};

struct edge_data_t : pair_edge_base<edge_data_t> {
  int id;
  capacity_t capacity;
  mutable capacity_t flow = 0;
};

using graph_t = graph<node_data_t, edge_data_t>;

template<typename... First>
struct readable_tuple : std::tuple<First...> {
  using std::tuple<First...>::tuple;

  template<size_t ind = 0>
  static void read(std::istream &in, std::tuple<First...> &tuple) {
    in >> std::get<ind>(tuple);
    if constexpr (ind + 1 != sizeof...(First))
      read<ind + 1>(in, tuple);
  }

  friend std::istream &operator>>(std::istream &in, readable_tuple &tuple) {
    read(in, tuple);
    return in;
  }
};

template<typename T>
struct counting_istream_iterator : supplier_iterator<counting_istream_iterator<T>, const T> {
  mutable std::istream_iterator<T> base;

  using sup_base_t = supplier_iterator<counting_istream_iterator, const T>;
  mutable bool value_ready = true;

  counting_istream_iterator(std::istream &in, int begin = 0) : sup_base_t(begin), base(in) {}
  counting_istream_iterator(int begin = 0) : sup_base_t(begin) {}
  const T &invoke() const override {
    if (!value_ready) {
      ++base;
      value_ready = true;
    }
    return *base;
  }
  void increment() override {
    if (!value_ready) {
      ++base;
    }
    value_ready = false;
  }
  using sup_base_t::operator==;
  using sup_base_t::operator!=;
  using sup_base_t::operator*;
  using sup_base_t::operator++;
};

using edge_descriptor = std::tuple<int, int, edge_data_t>;

struct edge_outer_supplier : supplier_iterator<edge_outer_supplier, const edge_descriptor> {
  using istream_iterator = counting_istream_iterator<readable_tuple<int, int,
                                                                    capacity_t>>;
  mutable istream_iterator cur;
  mutable int counter = 0;
  mutable edge_data_t tracker;
  mutable std::optional<edge_descriptor> data;
  capacity_t &max_cap;

  edge_outer_supplier(std::istream &in, capacity_t &max_cap)
      : cur(in), max_cap(max_cap) {}
  edge_outer_supplier(int m, capacity_t &max_cap)
      : cur(m), max_cap(max_cap) {}

  const edge_descriptor &invoke() const override {
    if (!data.has_value()) {
      switch (counter) {
      case 0:
        if (std::get<2>(*cur) > max_cap)
          max_cap = std::get<2>(*cur);
        data = edge_descriptor{std::get<0>(*cur) - 1, std::get<1>(*cur) - 1,
                               edge_data_t{&tracker, cur.sup_base_t::cur, std::get<2>(*cur)}};
        break;
      case 1:
        data = edge_descriptor{std::get<1>(*cur) - 1, std::get<0>(*cur) - 1,
                               edge_data_t{tracker.paired, -1, 0}};
        break;
      }
    }
    return *data;
  }
  void increment() override {
    counter++;
    if (counter == 2) {
      counter = 0;
      ++cur;
    }
    data.reset();
  }
  bool equals(const edge_outer_supplier &other) const override {
    return cur == other.cur;
  }
};

std::tuple<graph_t, std::vector<int>> read_graph(std::istream &in) {
  int n, m, L;
  if (!(in >> n >> m >> L)) {
    edge_descriptor *it = nullptr;
    int *nit = nullptr;
    return {graph_t(0, it, it, nit, nit), {}};
  }

  std::vector<int> v2l(n);
  for (int i = 0; i < n; i++) {
    in >> v2l[i];
    v2l[i]--;
  }

  capacity_t A = 0;
  graph_t g =
      graph_t(n, edge_outer_supplier(in, A), edge_outer_supplier(m, A), v2l.begin(), v2l.end());
  return {std::move(g), v2l};
}

void find_blocking_flow(const graph_t &gl, std::vector<int> v_asc_layer) {
  // Karzanov
  using adj_iter = typename decltype(gl.nodes[0].adjacent)::const_iterator;

  int s = v_asc_layer.front(), t = v_asc_layer.back();

  std::vector<bool> is_blocked(gl.size());
  int n_exc = 0;
  std::vector<adj_iter> first_in(gl.size()), first_out(gl.size());
  for (int v = 0; v < (int)gl.size(); v++)
    first_in[v] = first_out[v] = gl.nodes[v].adjacent.begin();

  auto change_excess_flow = [&](int v, capacity_t delta) {
    if (v != t && v != s) {
      bool old_exc = gl[v].excess_flow;
      gl[v].excess_flow += delta;
      n_exc += (0 + bool(gl[v].excess_flow) - old_exc);
    }
  };
  auto change_flow = [&](int u, int v, const edge_data_t &uv, capacity_t delta) {
    uv.flow += delta;
    uv.paired->flow -= delta;
    change_excess_flow(u, -delta);
    change_excess_flow(v, delta);
  };

  // generate blocking preflow from s
  for (auto &next : gl.nodes[s].adjacent) {
    change_flow(s, next.other_id, next.data, next.data.capacity);
  }
  is_blocked[s] = true;

  while (n_exc > 0) {
    // phase 1
    for (auto v : v_asc_layer)
      if (!is_blocked[v]) {
        for (auto it = first_out[v]; gl[v].excess_flow > 0 && it != gl.nodes[v].adjacent.end();
             ++it)
          if (it->data.id >= 0) {
            auto &vu = it->data;
            int u = it->other_id;
            if (!is_blocked[u]) {
              first_out[v] = it;
              capacity_t delta = std::min(gl[v].excess_flow, vu.capacity - vu.flow);
              change_flow(v, u, vu, delta);
            }
          }
        if (gl[v].excess_flow > 0)
          is_blocked[v] = true;
      }

    // phase 2
    for (auto vit = v_asc_layer.rbegin(); vit != v_asc_layer.rend(); ++vit) {
      int v = *vit;
      if (is_blocked[v]) {
        for (auto it = first_in[v]; gl[v].excess_flow > 0 && it != gl.nodes[v].adjacent.end(); ++it)
          if (it->data.id < 0) {
            auto &vu = it->data;
            auto &uv = *vu.paired;
            int u = it->other_id;
            if (uv.flow > 0) {
              first_in[v] = it;
              capacity_t delta = std::min(gl[v].excess_flow, uv.flow);
              change_flow(v, u, vu, delta);
            }
          }
        if (gl[v].excess_flow != 0)
          throw "a fit";
      }
    }
  }
}

int main() {
  auto[g, v2l] = read_graph(std::cin);
  int n = (int)g.size();

  std::vector<int> v_asc_layer(n);
  std::iota(v_asc_layer.begin(), v_asc_layer.end(), 0);
  std::sort(v_asc_layer.begin(), v_asc_layer.end(), [&](int u, int v) { return v2l[u] < v2l[v]; });

  find_blocking_flow(g, v_asc_layer);

  std::vector<int> e2f(g.n_of_edges() / 2);
  std::vector<bool> was(g.size());
  std::function<void(int)> dfs = [&](int v) {
    was[v] = true;
    if (v == v_asc_layer.back())
      return;
    for (auto &next : g.nodes[v].adjacent) {
      int u = next.other_id;
      auto &vu = next.data;
      if (g[u].layer == g[v].layer + 1) {
        e2f[vu.id] = vu.flow;
        if (!was[u])
          dfs(u);
      }
    }
  };

  dfs(v_asc_layer.front());

  for (auto el : e2f)
    std::cout << el << '\n';
  std::cout.flush();

  return 0;
}
