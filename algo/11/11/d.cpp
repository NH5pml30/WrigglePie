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

using node_data_t = empty;

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

struct edge_data_t : pair_edge_base<edge_data_t> {
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
    value_ready = false;
  }
  using sup_base_t::operator==;
  using sup_base_t::operator!=;
  using sup_base_t::operator*;
  using sup_base_t::operator++;
};

using edge_descriptor = std::tuple<int, int, edge_data_t>;

struct edge_outer_supplier : supplier_iterator<edge_outer_supplier, const edge_descriptor> {
  using istream_iterator = counting_istream_iterator<readable_tuple<int, int, capacity_t>>;
  mutable istream_iterator cur;
  istream_iterator::sup_base_t end;
  int counter = 0;
  mutable edge_data_t tracker;
  mutable std::optional<edge_descriptor> data;
  capacity_t &max;

  edge_outer_supplier(std::istream &in, int m, capacity_t &max) : cur(in), end(m), max(max) {}
  edge_outer_supplier(int m, capacity_t &max) : cur(m), end(m), max(max) {}

  const edge_descriptor &invoke() const override {
    if (!data.has_value()) {
      switch (counter) {
      case 0:
        if (std::get<2>(*cur) > max)
          max = std::get<2>(*cur);
        data = edge_descriptor{std::get<0>(*cur) - 1, std::get<1>(*cur) - 1,
                               edge_data_t{&tracker, std::get<2>(*cur)}};
        break;
      case 1:
        data = edge_descriptor{std::get<1>(*cur) - 1, std::get<0>(*cur) - 1,
                               edge_data_t{tracker.paired, 0}};
        break;
      }
    }
    return *data;
  }
  void increment() override {
    counter++;
    if (counter == 2 && cur != end) {
      counter = 0;
      ++cur;
    }
    data.reset();
  }
  bool equals(const edge_outer_supplier &other) const override {
    return cur == other.cur;
  }
};

std::pair<graph_t, capacity_t> read_graph(std::istream &in) {
  int n, m;
  if (!(in >> n >> m)) {
    edge_descriptor *it = nullptr;
    return {graph_t(0, it, it), 0};
  }

  capacity_t A = 0;
  graph_t g = graph_t(n, edge_outer_supplier(in, m, A), edge_outer_supplier(m, A));
  return {std::move(g), A};
}

template<typename ResT>
ResT Dinic(const graph_t &g, int s, int t, capacity_t A) {
  std::vector<int> dist(g.size());
  auto bfs = [&]() {
    std::queue<int> q;
    q.push(s);
    dist[s] = 0;
    while (!q.empty()) {
      int v = q.front();
      q.pop();
      for (auto &next : g.nodes[v].adjacent) {
        int u = next.other_id;
        auto &vu = next.data;
        if (vu.capacity - vu.flow >= A && dist[u] == std::numeric_limits<int>::max()) {
          dist[u] = dist[v] + 1;
          q.push(u);
        }
      }
    }

    return dist[t] != std::numeric_limits<int>::max();
  };

  std::vector<int> h(g.size());
  std::function<capacity_t(int, capacity_t)> dfs = [&](int v, capacity_t c_min) {
    if (v == t)
      return c_min;
    for (; h[v] < (int)g.nodes[v].adjacent.size(); h[v]++) {
      int u = g.nodes[v].adjacent[h[v]].other_id;
      auto &vu = g.nodes[v].adjacent[h[v]].data;
      auto &uv = *vu.paired;
      if (dist[u] == dist[v] + 1 && vu.capacity - vu.flow >= A) {
        capacity_t fp = dfs(u, std::min(c_min, vu.capacity - vu.flow));
        if (fp > 0) {
          vu.flow += fp;
          uv.flow -= fp;
          return fp;
        }
      }
    }
    return 0;
  };

  ResT res = 0;
  std::fill(dist.begin(), dist.end(), std::numeric_limits<int>::max());
  while (bfs()) {
    std::fill(h.begin(), h.end(), 0);
    capacity_t d;
    do {
      d = dfs(s, std::numeric_limits<capacity_t>::max());
      res += d;
    } while (d != 0);
    std::fill(dist.begin(), dist.end(), std::numeric_limits<int>::max());
  }
  return res;
}

template<typename ResT>
ResT scaling(const graph_t &g, int s, int t, capacity_t A) {
  A--;
  A |= A >> 1;
  A |= A >> 2;
  A |= A >> 4;
  A |= A >> 8;
  A |= A >> 16;
  A++;

  ResT res = 0;
  while (A > 0) {
    res += Dinic<ResT>(g, s, t, A);
    A /= 2;
  }

  return res;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto[g, A] = read_graph(std::cin);
  std::cout << scaling<long long>(g, 0, (int)g.size() - 1, A) << std::endl;
  return 0;
}
