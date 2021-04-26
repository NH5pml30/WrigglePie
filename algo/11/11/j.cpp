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

template<typename KeyT>
class binary_heap {
  using key_t = KeyT;

 public:
  struct DATA {
    key_t key;
    int id;

    DATA(key_t key, int id) : key(key), id(id) {}

    DATA() {}

    bool operator<(const DATA &other) const { return key < other.key; }

    bool operator>(const DATA &other) const { return key > other.key; }
  };

 private:
  std::vector<DATA> data;
  std::vector<int> idMap;

  void siftDown(int i) {
    for (int left = 2 * i + 1, right = 2 * i + 2;
         (left < (int)data.size() && data[i] > data[left]) ||
         (right < (int)data.size() && data[i] > data[right]);
         left = 2 * i + 1, right = 2 * i + 2) {
      int new_place;
      if (left >= (int)data.size())
        new_place = right;
      else if (right >= (int)data.size())
        new_place = left;
      else
        new_place = (data[left] < data[right] ? left : right);

      std::swap(idMap[data[i].id], idMap[data[new_place].id]);
      std::swap(data[i], data[new_place]);
      i = new_place;
    }
  }

  void siftUp(int i) {
    for (int parent = (i - 1) / 2; data[i] < data[parent];
         parent = (i - 1) / 2) {
      std::swap(idMap[data[i].id], idMap[data[parent].id]);
      std::swap(data[i], data[parent]);
      i = parent;
    }
  }

 public:
  binary_heap() {}

  void insert(key_t key, int id) {
    data.push_back(DATA(key, id));
    if (id >= (int)idMap.size()) idMap.resize(id + 1, -1);
    idMap[id] = (int)data.size() - 1;
    siftUp((int)data.size() - 1);
  }

  DATA extractMin() {
    DATA res = data.front();
    idMap[res.id] = -1;
    data.front() = data.back();
    data.pop_back();

    if (data.size() > 0) {
      idMap[data.front().id] = 0;
      siftDown(0);
    }
    return res;
  }

  size_t size() { return data.size(); }

  bool decreaseKey(int id, key_t newKey) {
    if (id >= (int)idMap.size() || idMap[id] == -1) return false;
    data[idMap[id]].key = newKey;
    siftUp(idMap[id]);
    return true;
  }
};

template <typename T = int>
constexpr T infinity = std::numeric_limits<T>::max();

template <typename T = int>
constexpr T neg_infinity = std::numeric_limits<T>::min();

template<typename T>
struct number {
  static constexpr T inf = infinity<T>, neg_inf = neg_infinity<T>;
  T data;

  number(T d) : data(d) {}

  number operator+(number other) const {
    if (other.data > 0 && data > inf - other.data) return inf;
    if (other.data < 0 && data < neg_inf - other.data) return neg_inf;
    return data + other.data;
  }
  number operator-(number other) const {
    if (other.data < 0 && data > inf + other.data) return inf;
    if (other.data > 0 && data < neg_inf + other.data) return neg_inf;
    return data - other.data;
  }
  number &operator+=(number other) {
    return *this = *this + other;
  }
  number &operator-=(number other) {
    return *this = *this - other;
  }

  bool operator<(number other) const {
    return data < other.data;
  }
  bool operator>(number other) const {
    return data > other.data;
  }
  bool is_inf() const {
    return data == inf;
  }
  bool is_neg_inf() const {
    return data == neg_inf;
  }
  bool is_nan() const {
    return is_inf() || is_neg_inf();
  }
  bool operator==(number other) const {
    return is_nan() || other.is_nan() ? false : data == other.data;
  }
  bool operator!=(number other) const {
    return is_nan() || other.is_nan() ? false : data != other.data;
  }
};

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

using cost_t = int32_t;
using edge_data_t = cost_t;

using edge_descriptor = std::tuple<int, int, edge_data_t>;

template<class NODE_DATA = empty, class EDGE_DATA = empty>
class bipartite_graph : public graph<NODE_DATA, EDGE_DATA> {
 public:
  using base = graph<NODE_DATA, EDGE_DATA>;
  int n1, n2;

  template<class EdgeIter>
  class bi_edge_supplier
      : public supplier_iterator<bi_edge_supplier<EdgeIter>, const std::remove_reference_t<decltype(
                                                                 *std::declval<EdgeIter>())>> {
    using edge_t = std::remove_reference_t<decltype(*std::declval<EdgeIter>())>;
    using base_t = supplier_iterator<bi_edge_supplier, edge_t>;
    int n1;
    EdgeIter data;
    mutable edge_t cached;

   public:
    bi_edge_supplier(int n1, EdgeIter data) : n1(n1), data(std::move(data)) {}
    const edge_t &invoke() const override {
      cached = *data;
      std::get<1>(cached) += n1;
      return cached;
    }
    void increment() override {
      ++data;
    }
    bool equals(const bi_edge_supplier &other) const override {
      return data == other.data;
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

using graph_t = bipartite_graph<node_data_t, edge_data_t>;

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
int main() {
  int n;
  std::cin >> n;
  std::vector<edge_descriptor> edges;
  edges.reserve(n * n);
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      int cost;
      std::cin >> cost;
      edges.push_back({i, j, cost});
    }
  }
  std::sort(edges.begin(), edges.end(),
            [&](auto &edge_l, auto &edge_r) { return std::get<2>(edge_l) < std::get<2>(edge_r); });

  int l = 0, r = (int)edges.size();  // f(l) == true, f(r) == false
  while (l < r - 1) {
    int m = (l + r) / 2;
    graph_t g = graph_t(n, n, edges.begin() + m, edges.end());
    if (HopcroftKarp(g, noop) == n) {
      // f(m) == true
      l = m;
    } else {
      // f(m) == false
      r = m;
    }
  }
  std::cout << std::get<2>(edges[l]) << std::endl;

  return 0;
}
