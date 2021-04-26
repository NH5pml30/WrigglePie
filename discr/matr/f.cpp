/* Kholiavin Nikolai, M3238 */
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
#include <set>
#include <fstream>

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

  void add_edge(int from, int to, EDGE_DATA data = {})
  {
    nodes[from].add_adjacent(to, std::move(data));
    n_of_edges_++;
  }
  void remove_edge(int from, int to)
  {
    auto it = std::find_if(nodes[from].adjacent.begin(), nodes[from].adjacent.end(),
                           [&](const auto &x) { return x.other_id == to; });
    if (it != nodes[from].adjacent.end())
    {
      nodes[from].adjacent.erase(it);
      n_of_edges_--;
    }
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
struct edge_data_t
{
  int id;
  int c;
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

struct edge_supplier : supplier_iterator<edge_supplier, const edge_descriptor>
{
  using istream_iterator = counting_istream_iterator<readable_tuple<int, int, int>>;
  mutable istream_iterator cur;
  mutable bool is_forw = true;
  mutable std::optional<edge_descriptor> data;

  edge_supplier(std::istream &in)
      : cur(in) {}
  edge_supplier(int m)
      : cur(m) {}

  const edge_descriptor &invoke() const override {
    if (!data.has_value())
      if (is_forw)
        data = edge_descriptor{std::get<0>(*cur) - 1, std::get<1>(*cur) - 1,
                               edge_data_t{cur.cur, std::get<2>(*cur)}};
      else
        data = edge_descriptor{std::get<1>(*cur) - 1, std::get<0>(*cur) - 1,
                               edge_data_t{cur.cur, std::get<2>(*cur)}};
    return *data;
  }
  void increment() override {
    if (is_forw)
      is_forw = false;
    else
    {
      ++cur;
      is_forw = true;
    }
    data.reset();
  }
  bool equals(const edge_supplier &other) const override {
    return cur == other.cur;
  }
};

graph_t read_graph(std::istream &in) {
  int n, m;
  if (!(in >> n >> m)) {
    edge_descriptor *it = nullptr;
    return graph_t(0, it, it);
  }

  return graph_t(n, edge_supplier(in), edge_supplier(m));
}

template<class Callback>
int max_colorful_forest(graph_t &g, Callback &&callback)
{
  std::list<edge_descriptor> edges_a, edges_not_a;
  for (auto &n : g.nodes)
    for (auto &next : n.adjacent)
      if (n.id < next.other_id)
        edges_not_a.push_back({n.id, next.other_id, next.data});

  using base_t = graph<int, edge_data_t>;
  base_t Ag(g.size(), (edge_descriptor *)(nullptr), (edge_descriptor *)(nullptr),
            counting_iterator_t(0), counting_iterator_t(g.size()));
  std::set<int> Ac;
  int next_id = g.size();

  std::vector<bool> used(Ag.size());
  auto get_from = [&](const edge_descriptor &e) { return std::get<0>(e); };
  auto get_to = [&](const edge_descriptor &e) { return std::get<1>(e); };
  auto get_c = [&](const edge_descriptor &e) { return std::get<2>(e).c; };
  auto get_id = [&](const edge_descriptor &e) { return std::get<2>(e).id; };
  auto ag_color_dfs = [&](int start, int col) {
    std::fill(used.begin(), used.end(), false);
    std::function<void(int)> dfs = [&](int v) {
      used[v] = true;
      Ag[v] = col;
      for (auto &next : Ag.nodes[v].adjacent)
        if (!used[next.other_id])
          dfs(next.other_id);
    };
    dfs(start);
  };
  auto ag_remove = [&](const edge_descriptor &e) {
    int from = get_from(e), to = get_to(e);
    Ag.remove_edge(from, to);
    Ag.remove_edge(to, from);
    ag_color_dfs(to, next_id++);
  };
  auto ag_can_add = [&](const edge_descriptor &e) {
    int from = get_from(e), to = get_to(e);
    return Ag[from] != Ag[to];
  };
  auto ag_add = [&](const edge_descriptor &e) {
    int from = get_from(e), to = get_to(e);
    Ag.add_edge(from, to);
    Ag.add_edge(to, from);
    ag_color_dfs(to, Ag[from]);
  };

  auto ac_remove = [&](const edge_descriptor &e) { Ac.erase(get_c(e)); };
  auto ac_can_add = [&](const edge_descriptor &e) { return !Ac.count(get_c(e)); };
  auto ac_add = [&](const edge_descriptor &e) { Ac.insert(get_c(e)); };

  using edge_iter = std::list<edge_descriptor>::const_iterator;

  auto bfs = [&](const std::vector<edge_iter> &S, const std::vector<bool> &is_in_T) {
    std::vector<std::pair<int, edge_iter>> dist(g.n_of_edges() / 2, {std::numeric_limits<int>::max(), edges_a.end()});
    std::queue<std::pair<edge_iter, bool>> q;
    for (auto &s : S)
    {
      dist[get_id(*s)] = {0, edges_a.end()};
      if (is_in_T[get_id(*s)])
        return std::make_pair(dist, edge_iter(s));
      q.push({s, false});
    }

    while (!q.empty())
    {
      auto [cur, is_in_a] = q.front();
      int v = get_id(*cur);
      q.pop();
      if (is_in_a)
      {
        // try edges with graph matroid
        ag_remove(*cur);
        for (auto next_it = edges_not_a.cbegin(); next_it != edges_not_a.cend(); next_it++)
          if (auto next = get_id(*next_it); dist[next].first > dist[v].first + 1)
          {
            if (ag_can_add(*next_it))
            {
              dist[next] = {dist[v].first + 1, cur};
              q.push({next_it, false});
              if (is_in_T[next])
                return std::make_pair(dist, next_it);
            }
          }
        ag_add(*cur);  // roll back
      }
      else
      {
        // try edges with colorful matroid
        for (auto next_it = edges_a.cbegin(); next_it != edges_a.cend(); next_it++)
          if (auto next = get_id(*next_it); dist[next].first > dist[v].first + 1)
          {
            ac_remove(*next_it);
            if (ac_can_add(*cur))
            {
              dist[next] = {dist[v].first + 1, cur};
              q.push({next_it, true});
            }
            ac_add(*next_it);
          }
      }
    }
    return std::make_pair(dist, edges_not_a.cend());
  };

  auto find_S = [&] {
    std::vector<edge_iter> S;
    for (auto next_it = edges_not_a.begin(); next_it != edges_not_a.end(); next_it++)
      if (ag_can_add(*next_it))
        S.push_back(next_it);
    return S;
  };
  auto find_T = [&] {
    std::vector<bool> is_in_T(g.n_of_edges() / 2);
    for (auto next_it = edges_not_a.begin(); next_it != edges_not_a.end(); next_it++)
      if (ac_can_add(*next_it))
        is_in_T[get_id(*next_it)] = true;
    return is_in_T;
  };

  while (true)
  {
    auto S = find_S();
    auto is_in_T = find_T();
    auto &&[dist, t] = bfs(S, is_in_T);
    if (t == edges_not_a.end())
    {
      // A is max now
      for (auto &e : edges_a)
        callback(e);
      return edges_a.size();
    }

    // A is not max, found path S~>T, alternate it
    edge_iter cur = t;
    while (dist[get_id(*cur)].second != edges_a.end())
    {
      auto prev = dist[get_id(*cur)].second;
      cur = prev;
      prev = dist[get_id(*cur)].second;
      ag_remove(*cur);
      ac_remove(*cur);
      cur = prev;
    }
    cur = t;
    while (dist[get_id(*cur)].second != edges_a.end())
    {
      auto prev = dist[get_id(*cur)].second;
      ag_add(*cur);
      ac_add(*cur);
      cur = prev;
      prev = dist[get_id(*cur)].second;
      cur = prev;
    }
    ag_add(*cur);
    ac_add(*cur);
    cur = t;
    while (dist[get_id(*cur)].second != edges_a.end())
    {
      auto prev = dist[get_id(*cur)].second;
      edges_a.splice(edges_a.end(), edges_not_a, cur);
      cur = prev;
      prev = dist[get_id(*cur)].second;
      edges_not_a.splice(edges_not_a.end(), edges_a, cur);
      cur = prev;
    }
    edges_a.splice(edges_a.end(), edges_not_a, cur);
  }
}

int main()
{
#ifdef _DEBUG
  auto &in = std::cin;
  auto &out = std::cout;
#else
  auto in = std::ifstream("rainbow.in");
  auto out = std::ofstream("rainbow.out");
#endif

  graph_t g = read_graph(in);
  std::vector<int> edges;
  int k =
      max_colorful_forest(g, [&](const edge_descriptor &e) { edges.push_back(std::get<2>(e).id); });
  out << k << std::endl;
  for (auto el : edges)
    out << el + 1 << ' ';
  out << std::endl;
}
