/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <optional>
#include <numeric>
#include <algorithm>
#include <functional>

struct empty {};

template<class ChildT>
struct counting_iterator {
  using iterator_category = std::forward_iterator_tag;
  using value_type        = int;
  using difference_type   = int;
  using pointer           = int *;
  using reference         = int &;

  int cur = 0;
  std::function<ChildT(counting_iterator &&)> factory;

  template<class ChildFactory>
  counting_iterator(int begin, ChildFactory factory) : cur(begin), factory(std::move(factory)) {}
  counting_iterator() : counting_iterator(0) {}
  counting_iterator(int begin)
      : cur(begin), factory([](counting_iterator &&base) { return ChildT(std::move(base)); }) {}
  virtual void increment() {}
  ChildT &operator++() {
    cur++;
    increment();
    return static_cast<ChildT &>(*this);
  }
  ChildT operator++(int) {
    auto res = factory(counting_iterator(cur, factory));
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
};

struct counting_iterator_t : counting_iterator<counting_iterator_t> {
  using base_t = counting_iterator<counting_iterator_t>;
  using base_t::counting_iterator;
  counting_iterator_t(base_t &&base) : base_t(std::move(base)) {}
};

template<class T>
struct supplier_iterator : counting_iterator<supplier_iterator<T>> {
  using base_t = counting_iterator<supplier_iterator>;
  using value_type = T;
  using pointer    = T *;
  using reference  = T &;

  supplier_iterator(int begin = 0) : base_t(begin) {}
  supplier_iterator(base_t &&base) : base_t(std::move(base)) {}
  virtual T invoke() const {
    return T();
  }
  auto operator*() const {
    return invoke();
  }
};

using empty_supplier = supplier_iterator<empty>;

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

  template<class T, class ChildT>
  struct base_graph_iterator : supplier_iterator<T> {
    using base_t = supplier_iterator<T>;
    ChildT *enclosing;

    base_graph_iterator(ChildT *enclosing, int begin = 0)
        : base_t(begin), enclosing(enclosing) {}
  };

  template<class ChildT>
  struct node_iterator : base_graph_iterator<NODE_DATA, ChildT> {
    using base_t = base_graph_iterator<NODE_DATA, ChildT>;
    using base_t::base_graph_iterator;

    NODE_DATA invoke() const override {
      return base_t::enclosing->nodes[base_t::cur].data;
    }
  };

  std::vector<NODE> nodes;
  int n_of_edges_;

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
    graph res;
    res.resize(size());
    for (auto &n : nodes)
      for (auto &e : n.adjacent) res.add_edge(e.other_id, n.id, e.data);
    return res;
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
  using base = graph<NODE_DATA, EDGE_DATA>;
  int n1, n2;

 public:
  template<class EdgeIter, class NodeIter = empty_supplier>
  bipartite_graph(int n1, int n2, EdgeIter edge_begin, EdgeIter edge_end,
                  NodeIter node_begin = empty_supplier(0), NodeIter node_end = empty_supplier(0))
      : base(n1 + n2, edge_begin, edge_end, node_begin, node_end),
        n1(n1),
        n2(n2) {}

  bipartite_graph complement() const {
    std::vector<std::vector<int>> edges;
    using edge_t =
        std::conditional_t<std::is_same_v<EDGE_DATA, empty>,
            std::tuple<int, int>, std::tuple<int, int, EDGE_DATA>>;
    struct complement_edge_supplier
        : base::template base_graph_iterator<edge_t, const bipartite_graph> {
      using base_t = typename base::template
        base_graph_iterator<edge_t, const bipartite_graph>;
      int cur_left_v;
      std::vector<int> right_pending;

      complement_edge_supplier(const bipartite_graph *enclosing, int node = 0, int order = 0)
          : base_t(enclosing, order), cur_left_v(node - 1) {
        increment();
      }

      edge_t invoke() const override {
        if constexpr (std::is_same_v<EDGE_DATA, empty>)
          return {cur_left_v, right_pending.back() + base_t::enclosing->n1};
        else
          return {cur_left_v, right_pending.back() + base_t::enclosing->n1, EDGE_DATA()};
      }
      void increment() override {
        if (!right_pending.empty())
          right_pending.pop_back();
        while (right_pending.empty()) {
          cur_left_v++;
          if (cur_left_v < base_t::enclosing->n1) {
            integer_set_complement(
                base_t::enclosing->n2, base_t::enclosing->nodes[cur_left_v].adjacent.begin(),
                base_t::enclosing->nodes[cur_left_v].adjacent.end(),
                std::back_inserter(right_pending),
                [&](const auto &edge) { return edge.other_id - this->enclosing->n1; });
          } else {
            break;
          }
        }
      }
    };

    using node_iterator = typename base::template node_iterator<const bipartite_graph>;

    return bipartite_graph(n1, n2, complement_edge_supplier(this),
                           complement_edge_supplier(this, n1, n1 * n2 - base::n_of_edges_),
                           node_iterator(this),
                           node_iterator(this, n1 + n2));
  }

  template<class MatchCallback>
  int max_matching(MatchCallback callback) const {
    std::vector<int> matches(n2, -1);
    int m = 0;

    std::vector<bool> used(n1);
    std::function<bool(int)> dfs = [&](int v) -> bool {
      used[v] = true;
      for (auto &next : base::nodes[v].adjacent) {
        int u = next.other_id - n1;
        if (matches[u] == -1 ||
            (!used[matches[u]] && dfs(matches[u]))) {
          matches[u] = v;
          return true;
        }
      }
      return false;
    };

    for (int v = 0; v < n1; v++) {
      std::fill(used.begin(), used.end(), false);
      m += dfs(v);
    }

    for (int i = 0; i < n2; i++)
      if (matches[i] != -1)
        callback(matches[i], i);
    return m;
  }

  template<class VertexCallback>
  int build_min_vertex_cover(std::vector<int> &max_matching, VertexCallback callback) const {
    std::vector<int> r2l_match(n2, -1);
    for (int v = 0; v < n1; v++)
      if (max_matching[v] != -1)
        r2l_match[max_matching[v]] = v;

    std::vector<bool> visited_l(n1), visited_r(n2);
    std::function<void(int)> dfs = [&](int v) {
      visited_l[v] = true;
      for (auto &next : base::nodes[v].adjacent) {
        int u = next.other_id - n1;
        if (max_matching[v] != u && !visited_r[u]) {
          visited_r[u] = true;
          dfs(r2l_match[u]);
        }
      }
    };

    int c = 0;
    for (int v = 0; v < n1; v++)
      if (!visited_l[v] && max_matching[v] == -1)
        dfs(v);

    for (int v = 0; v < n1; v++)
      if (!visited_l[v]) {
        callback(v);
        c++;
      }
    for (int v = 0; v < n2; v++)
      if (visited_r[v]) {
        callback(n1 + v);
        c++;
      }
    return c;
  }
};

using NODE_DATA_T = empty;
using EDGE_DATA_T = empty;

using graph_t = bipartite_graph<NODE_DATA_T, EDGE_DATA_T>;

std::pair<graph_t, int> read_graph(std::istream &in) {
  int n, m;
  in >> m >> n;

  std::vector<std::pair<int, int>> edges;
  for (int i = 0; i < m; i++) {
    while (true) {
      int to;
      in >> to;
      if (to == 0)
        break;
      to = m + to - 1;
      edges.push_back({i, to});
    }
  }

  return {graph_t(m, n, edges.begin(), edges.end()), m};
}

int main() {
  int k;
  std::cin >> k;
  for (int i = 0; i < k; i++) {
    auto[g, n1] = read_graph(std::cin);
    // find bipartite graph complement (complementing only vertices from different halves)
    g = g.complement();
    // find max matching of complement
    std::vector<int> matching_c(n1, -1);
    g.max_matching([&](int from, int to) { matching_c[from] = to; });
    // find min vertex cover of complement
    std::vector<int> vertex_cover_c;
    using namespace std::placeholders;
    g.build_min_vertex_cover(matching_c, [&](int x) { vertex_cover_c.push_back(x); });
    // find max anticlique by complementing min vertex cover
    std::vector<int> anticlique_c;
    integer_set_complement((int)g.size(), vertex_cover_c.begin(), vertex_cover_c.end(),
                           std::back_inserter(anticlique_c), [](auto x) { return x; });
    // find max clique (discarding that vertices from one part are disconnected) in initial graph
    std::vector<int> clique = anticlique_c;

    std::cout << clique.size() << std::endl;
    auto divide_at = std::lower_bound(clique.begin(), clique.end(), n1);
    std::cout << divide_at - clique.begin() << ' ' << clique.end() - divide_at << std::endl;
    for (auto it = clique.begin(); it != divide_at; it++)
      std::cout << *it + 1 << ' ';
    std::cout << std::endl;
    for (auto it = divide_at; it != clique.end(); it++)
      std::cout << *it + 1 - n1 << ' ';
    std::cout << std::endl;
  }

  return 0;
}
