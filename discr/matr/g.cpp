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
#include <map>
#include <fstream>

class disjoint_set {
 private:
  struct NODE_DATA {
    int index, parent, rank;

    NODE_DATA(int index) :
      index(index), parent(index), rank(0) {
    }

    NODE_DATA() {
    }

    static void attach(NODE_DATA &X, NODE_DATA &Y) {
      if (X.rank > Y.rank) {
        Y.parent = X.index;
      } else {
        X.parent = Y.index;
        if (X.rank == Y.rank)
          Y.rank++;
      }
    }
  };

  std::vector<NODE_DATA> nodes;

 public:
  disjoint_set(int n) : nodes(n) {
    int i = 0;
    for (auto &el : nodes)
      el = NODE_DATA(i++);
  }

  int find(int x) {
    if (nodes[x].parent == x)
      return x;
    return nodes[x].parent = find(nodes[x].parent);
  }

  void join(int x, int y) {
    x = find(x), y = find(y);
    if (x == y)
      return;

    NODE_DATA::attach(nodes[x], nodes[y]);
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

template<class NODE_DATA = empty, class EDGE_DATA = empty, template<typename> class Container = std::list>
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
        return other_id < other.other_id;
      }

      bool operator<(int other_other_id) const {
        return other_id < other_other_id;
      }
    };
    int id;
    NODE_DATA data;
    std::multiset<EDGE_ENTRY> adjacent;

    NODE(int id = -1) : id(id) {}
    NODE(int id, NODE_DATA data) : id(id), data(std::move(data)) {}

    void add_adjacent(int other_id, EDGE_DATA data) {
      adjacent.insert(EDGE_ENTRY(other_id, std::move(data)));
    }
    bool remove_adjacent(int other_id) {
      return adjacent.erase({other_id, {}});
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

  void add_edge(int from, int to, EDGE_DATA data = {})
  {
    nodes[from].add_adjacent(to, std::move(data));
    n_of_edges_++;
  }
  void remove_edge(int from, int to)
  {
    n_of_edges_ -= nodes[from].remove_adjacent(to);
  }

  int size() const {
    return (int)nodes.size();
  }
  int n_of_edges() const {
    return n_of_edges_;
  }

  void clear()
  {
    for (auto &n : nodes)
      n.adjacent.clear();
    n_of_edges_ = 0;
  }

  std::optional<EDGE_DATA> get_edge(int from, int to) const {
    auto it = std::find_if(nodes[from].adjacent.begin(), nodes[from].adjacent.end(),
                           [&](const auto &x) { return x.other_id == to; });
    if (it == nodes[from].adjacent.end())
      return {};
    return it->data;
  }

  template<class Callback>
  void spanning_tree(Callback &&callback) const {
    // Kruskal without sort
    struct edge_entry {
      int u, w;
      EDGE_DATA data;
    };
    std::vector<edge_entry> edges;
    for (auto &u : nodes)
      for (auto &uv : u.adjacent)
        if (u.id < uv.other_id)
          edges.push_back({u.id, uv.other_id, uv.data});

    disjoint_set comps(nodes.size());
    int m = 0;
    for (auto &uw : edges) {
      if (comps.find(uw.u) != comps.find(uw.w)) {
        // Edge is safe, add to mst
        comps.join(uw.u, uw.w);
        m++;
        callback(uw.u, uw.w, uw.data);
        if (m == (int)nodes.size() - 1) break;
      }
    }
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

struct edge_descriptor
{
  int from, to;
  edge_data_t data;

  int get_id() const
  {
    return data.id;
  }
};

struct edge_supplier : supplier_iterator<edge_supplier, const edge_descriptor>
{
  using istream_iterator = counting_istream_iterator<readable_tuple<int, int>>;
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
                               edge_data_t{cur.cur}};
      else
        data = edge_descriptor{std::get<1>(*cur) - 1, std::get<0>(*cur) - 1,
                               edge_data_t{cur.cur}};
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

struct uniting_graph_matroid
{
  using graph_data_t = graph<int, edge_data_t>;
  std::reference_wrapper<const graph_t> g;
  graph_data_t Base;
  std::set<int> chosen_edges;
  int next_id;

  void constr_add_edge(int id, int base, std::vector<std::set<int>> &E2ind, std::set<int> &in_no_bases,
                       std::set<int> &in_multiple_bases)
  {
    E2ind[id].insert(base);
    if (E2ind[id].size() > 1)
      in_multiple_bases.insert(id);
    else if (E2ind[id].size() == 1)
      in_no_bases.erase(id);
  }

  uniting_graph_matroid(const graph_t &g, int ind, std::vector<std::set<int>> &E2ind,
                        std::set<int> &in_no_bases, std::set<int> &in_multiple_bases)
      : g(g),
        Base(g.size(), (edge_descriptor *)(nullptr), (edge_descriptor *)(nullptr),
             counting_iterator_t(0), counting_iterator_t(g.size())),
        next_id(g.size())
  {
    g.spanning_tree([&](int from, int to, edge_data_t data) {
      Base.add_edge(from, to, data);
      Base.add_edge(to, from, data);
      Base[to] = Base[from] = next_id;
      chosen_edges.insert(data.id);
      constr_add_edge(data.id, ind, E2ind, in_no_bases, in_multiple_bases);
    });
    next_id++;
  }

  uniting_graph_matroid(const uniting_graph_matroid &other, int ind,
                        std::vector<std::set<int>> &E2ind, std::set<int> &in_no_bases,
                        std::set<int> &in_multiple_bases)
      : uniting_graph_matroid(other)
  {
    for (auto &n : Base.nodes)
      for (auto &adj : n.adjacent)
        if (n.id < adj.other_id)
          constr_add_edge(adj.data.id, ind, E2ind, in_no_bases, in_multiple_bases);
  }

  void color_dfs(int v, int from_col, int to_col)
  {
    Base[v] = to_col;
    for (auto &next : Base.nodes[v].adjacent)
      if (Base[next.other_id] == from_col)
        color_dfs(next.other_id, from_col, to_col);
  }

  bool can_add(const edge_descriptor &e) const
  {
    return Base[e.from] != Base[e.to];
  }

  void add(const edge_descriptor &e)
  {
    Base.add_edge(e.from, e.to, e.data);
    Base.add_edge(e.to, e.from, e.data);
    chosen_edges.insert(e.data.id);
    color_dfs(e.to, Base[e.from], Base[e.to]);
  }

  void remove(const edge_descriptor &e)
  {
    Base.remove_edge(e.from, e.to);
    Base.remove_edge(e.to, e.from);
    chosen_edges.erase(e.data.id);
    color_dfs(e.to, Base[e.to], next_id++);
  }

  struct not_iterator
  {
    std::set<int>::iterator data, set_end;
    int cur;

    not_iterator(int cur, std::set<int>::iterator data, std::set<int>::iterator set_end) : cur(cur), data(data), set_end(set_end)
    {
      while (data != set_end && *data == cur)
        ++data, ++cur;
    }
    bool operator==(const not_iterator &other)
    {
      return cur == other.cur;
    }
    bool operator!=(const not_iterator &other)
    {
      return !operator==(other);
    }
    not_iterator &operator++()
    {
      if (data != set_end && *data == cur)
        while (data != set_end && *data == cur)
          ++data, ++cur;
      else
        ++cur;
      return *this;
    }
    not_iterator operator++(int)
    {
      not_iterator copy(*this);
      operator++();
      return copy;
    }
    int operator*() const
    {
      return cur;
    }
  };

  not_iterator begin_complement()
  {
    return not_iterator(0, chosen_edges.begin(), chosen_edges.end());
  }
  not_iterator end_complement()
  {
    return not_iterator(g.get().n_of_edges() / 2, chosen_edges.end(), chosen_edges.end());
  }
};

template<class Element, class Matroid>
int unite_matroids(std::vector<Matroid> &M, const std::vector<Element> &X,
                   std::vector<std::set<int>> &X2bases, std::set<int> &in_no_bases,
                   std::set<int> &in_multiple_bases, int A_size = 0)
{
  using element_iter = typename std::list<Element>::const_iterator;

  auto remove_from_base = [&](int base, int el) {
    M[base].remove(X[el]);
    X2bases[el].erase(base);
    if (X2bases[el].size() == 1)
      in_multiple_bases.erase(el);
    else if (X2bases[el].empty())
      in_no_bases.insert(el);
  };
  auto add_to_base = [&](int base, int el) {
    M[base].add(X[el]);
    X2bases[el].insert(base);
    if (X2bases[el].size() > 1)
      in_multiple_bases.insert(el);
    else if (X2bases[el].size() == 1)
      in_no_bases.erase(el);
  };

  struct path_edge_data
  {
    int dist;
    int last;
    int from_base;
  };
  std::vector<path_edge_data> dist(X.size());
  std::deque<int> q;
  auto bfs = [&]() {
    std::fill(dist.begin(), dist.end(), path_edge_data{std::numeric_limits<int>::max(), -1, -1});
    q.clear();
    for (auto s : in_multiple_bases)
    {
      dist[s] = {0, -1, -1};
      q.push_back(s);
    }

    while (!q.empty())
    {
      int v = q.front();
      q.pop_front();
      for (auto base : X2bases[v])
      {
        // try reversed edges with this matroid
        M[base].remove(X[v]);
        for (auto u_it = M[base].begin_complement(); u_it != M[base].end_complement(); ++u_it)
          if (int u = *u_it; dist[u].dist > dist[v].dist + 1 && M[base].can_add(X[u]))
          {
            dist[u] = {dist[v].dist + 1, v, base};
            q.push_back(u);
            if (X2bases[u].size() == 0)
              return u;
          }
        M[base].add(X[v]);
      }
    }
    return -1;
  };

  while (true)
  {
    int t = bfs();
    if (t == -1)
      // Union is max now
      return A_size;

    // Union is not max, found reversed path v \in (X \ U b[i]) ~> u \in >1 b[i], alternate it
    int cur = t;
    while (dist[cur].last != -1)
    {
      auto [_, prev, base] = dist[cur];
      remove_from_base(base, prev);
      cur = prev;
    }
    cur = t;
    while (dist[cur].last != -1)
    {
      auto [_, prev, base] = dist[cur];
      add_to_base(base, cur);
      cur = prev;
    }
    A_size++;
  }
}

std::vector<uniting_graph_matroid> max_nonintersecting_spans(const graph_t &g)
{
  std::vector<edge_descriptor> edges(g.n_of_edges() / 2);
  std::vector<uniting_graph_matroid> M, last_M;
  std::vector<std::set<int>> E2ind(g.n_of_edges() / 2);
  std::set<int> in_no_bases;
  std::set<int> in_multiple_bases;

  for (auto &n : g.nodes)
    for (auto &adj : n.adjacent)
      if (n.id < adj.other_id)
        edges[adj.data.id] = {n.id, adj.other_id, adj.data};

  int k, size = g.size() - 1;
  for (k = 0; ; k++)
  {
    last_M = M;
    if (k == 0)
      M.emplace_back(g, k, E2ind, in_no_bases, in_multiple_bases);
    else
      M.emplace_back(M.back(), k, E2ind, in_no_bases, in_multiple_bases);

    size = unite_matroids(M, edges, E2ind, in_no_bases, in_multiple_bases, size);
    if (size != (k + 1) * (g.size() - 1))
      break;
  }
  return last_M;
}

int main()
{
#ifdef _DEBUG
  auto &in = std::cin;
  auto &out = std::cout;
#else
  auto in = std::ifstream("multispan.in");
  auto out = std::ofstream("multispan.out");
#endif

  graph_t g = read_graph(in);
  auto trees = max_nonintersecting_spans(g);

  out << trees.size() << '\n';
  for (auto &tree : trees)
  {
    for (auto &n : tree.Base.nodes)
      for (auto &adj : n.adjacent)
        if (n.id < adj.other_id)
          out << adj.data.id + 1 << ' ';
    out << '\n';
  }

  out.flush();
  return 0;
}
