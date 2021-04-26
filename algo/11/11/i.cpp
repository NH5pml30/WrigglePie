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

using capacity_t = int32_t;
using cost_t = int32_t;

struct edge_data_t : pair_edge_base<edge_data_t> {
  capacity_t capacity;
  cost_t cost;
  mutable capacity_t flow = 0;

  operator cost_t() const {
    return cost;
  }
};

using graph_t = graph<node_data_t, edge_data_t>;


using edge_descriptor = std::tuple<int, int, edge_data_t>;

template<typename dist_t, class NODE_DATA, class EDGE_DATA, class EdgeLen>
std::enable_if_t<std::is_convertible_v<EDGE_DATA, dist_t>,
    std::vector<std::tuple<number<dist_t>, int, const EDGE_DATA *>>>
bellman_ford(const graph<NODE_DATA, EDGE_DATA> &g, int start, EdgeLen &&edge_len) {
  std::vector<std::tuple<number<dist_t>, int, const EDGE_DATA *>> dist(
      g.size(), {infinity<dist_t>, -1, nullptr});
  dist[start] = {0, -1, nullptr};

  for (int k = 0; k < (int)g.size() - 1; k++)
    for (auto &u : g.nodes)
      if (!std::get<0>(dist[u.id]).is_inf()) {
        for (auto &uv : u.adjacent) {
          if (auto loc_dist = std::get<0>(dist[u.id]) + edge_len(u.id, uv.other_id, uv.data);
              loc_dist < std::get<0>(dist[uv.other_id]))
            dist[uv.other_id] = {loc_dist, u.id, &uv.data};
        }
      }

  return dist;
}

template<typename dist_t, class NODE_DATA, class EDGE_DATA, class EdgeLen>
std::enable_if_t<std::is_convertible_v<EDGE_DATA, dist_t>,
    std::vector<std::tuple<number<dist_t>, int, const EDGE_DATA *>>>
dijkstra(const graph<NODE_DATA, EDGE_DATA> &g, int start, EdgeLen &&edge_len) {
  std::vector<std::tuple<number<dist_t>, int, const EDGE_DATA *>> dist(
      g.size(), {infinity<dist_t>, -1, nullptr});
  dist[start] = {0, -1, nullptr};
  binary_heap<number<dist_t>> prior_queue;
  prior_queue.insert(0, start);

  while (prior_queue.size() != 0) {
    int v = prior_queue.extractMin().id;
    // relax
    for (auto &next : g.nodes[v].adjacent)
      if (auto loc_dist = std::get<0>(dist[v]) + edge_len(v, next.other_id, next.data);
          loc_dist < std::get<0>(dist[next.other_id])) {
        dist[next.other_id] = {loc_dist, v, &next.data};
        if (!prior_queue.decreaseKey(next.other_id, loc_dist))
          prior_queue.insert(loc_dist, next.other_id);
      }
  }
  return dist;
}


template<typename flow_cost_t>
flow_cost_t max_flow_min_cost(const graph_t &gf, int s, int t) {
  std::vector<number<cost_t>> phi(gf.size(), 0);
  auto edge_cost = [&](int from, int to, const edge_data_t &e) -> number<cost_t> {
    if (e.capacity > e.flow) {
      if (phi[from] + e.cost - phi[to] < 0)
        throw "darou";
      return phi[from] + e.cost - phi[to];
    }
    return infinity<cost_t>;
  };
  auto d = bellman_ford<cost_t>(gf, s, edge_cost);
  for (int i = 0; i < (int)gf.size(); i++)
    phi[i] = std::get<0>(d[i]);

  flow_cost_t res = 0;
  while (true) {
    capacity_t fp = infinity<capacity_t>;
    if (std::get<0>(d[t]).is_inf())
      return res;

    int cur = t;
    while (cur != s) {
      fp = std::min(fp, std::get<2>(d[cur])->capacity - std::get<2>(d[cur])->flow);
      cur = std::get<1>(d[cur]);
    }
    cur = t;
    while (cur != s) {
      std::get<2>(d[cur])->flow += fp;
      std::get<2>(d[cur])->paired->flow -= fp;
      res += flow_cost_t{1} * fp * std::get<2>(d[cur])->cost;
      cur = std::get<1>(d[cur]);
    }

    d = dijkstra<cost_t>(gf, s, edge_cost);
    for (int v = 0; v < (int)gf.size(); v++)
      phi[v] += std::get<0>(d[v]);
  }
}

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

template<class NODE_DATA, class EDGE_DATA, class MatchCallback>
int remove_max_matching(bipartite_graph<NODE_DATA, EDGE_DATA> &g, MatchCallback callback) {
  using edge_iter = typename decltype(g.nodes[0].adjacent)::iterator;
  std::vector<std::pair<int, edge_iter>> matches(g.n2, {-1, {}});
  int m = 0;

  std::vector<bool> used(g.n1);
  auto dfs = [&](int v, auto dfs) -> bool {
    used[v] = true;
    for (auto next_it = g.nodes[v].adjacent.begin(); next_it != g.nodes[v].adjacent.end();
         ++next_it) {
      auto &next = *next_it;
      int u = next.other_id - g.n1;
      if (matches[u].first == -1 || (!used[matches[u].first] && dfs(matches[u].first, dfs))) {
        matches[u] = {v, next_it};
        return true;
      }
    }
    return false;
  };

  for (int v = 0; v < g.n1; v++) {
    std::fill(used.begin(), used.end(), false);
    m += dfs(v, dfs);
  }

  for (int i = 0; i < g.n2; i++)
    if (matches[i].first != -1) {
      callback(matches[i].first, i);
      g.nodes[matches[i].first].adjacent.erase(matches[i].second);
      g.n_of_edges_--;
    }
  return m;
}

using bip_graph_t = bipartite_graph<>;

int main() {
  int n, k;
  std::cin >> n >> k;
  std::vector<std::vector<int>> costs(n);
  std::vector<edge_descriptor> edges;
  edges.reserve(2 * (n + 2) * n);
  for (int i = 0; i < n; i++) {
    costs[i].resize(n);
    for (int j = 0; j < n; j++) {
      std::cin >> costs[i][j];
      edges.push_back({i, n + j, {nullptr, 1, costs[i][j]}});
      edges.push_back({n + j, i, {&std::get<2>(edges.back()), 0, -costs[i][j]}});
    }
  }
  for (int i = 0; i < n; i++) {
    edges.push_back({2 * n, i, {nullptr, k, 0}});
    edges.push_back({i, 2 * n, {&std::get<2>(edges.back()), 0, 0}});
  }
  for (int j = 0; j < n; j++) {
    edges.push_back({n + j, 2 * n + 1, {nullptr, k, 0}});
    edges.push_back({2 * n + 1, n + j, {&std::get<2>(edges.back()), 0, 0}});
  }

  graph_t g = graph_t(2 * n + 2, edges.begin(), edges.end());
  uint64_t res;
  std::cout << (res = max_flow_min_cost<uint64_t>(g, 2 * n, 2 * n + 1)) << std::endl;

  std::vector<std::pair<int, int>> bip_edges;
  bip_edges.reserve(n);
  for (int i = 0; i < n; i++) {
    for (auto &next : g.nodes[i].adjacent)
      if (next.other_id >= n && next.other_id < 2 * n && next.data.flow > 0)
        bip_edges.push_back({i, next.other_id - n});
  }

  uint64_t w = 0;
  bip_graph_t graph(n, n, bip_edges.begin(), bip_edges.end());
  for (int iter = 0; iter < k; iter++) {
    std::vector<int> match_l_to_r(n);
    remove_max_matching(graph, [&](int l, int r) { match_l_to_r[l] = r; });
    int i = 0;
    for (auto el : match_l_to_r) {
      std::cout << el + 1 << ' ';
      w += costs[i++][el];
    }
    std::cout << std::endl;
  }

  return 0;
}
