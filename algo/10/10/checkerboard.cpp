/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <optional>
#include <numeric>
#include <algorithm>
#include <functional>
#include <cassert>

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

  template<class EdgeIter>
  class bi_edge_supplier
      : base::template base_graph_iterator<decltype(*std::declval<EdgeIter>), bipartite_graph> {
   private:
    using edge_t = decltype(*std::declval<EdgeIter>());
    using base_t = typename base::template base_graph_iterator<edge_t, bipartite_graph>;
    EdgeIter data;

   public:
    bi_edge_supplier(EdgeIter data, int begin = 0) : base_t(begin), data(data) {}
    edge_t invoke() const override {
      auto res = *data;
      std::get<1>(res) += base_t::enclosing->n1;
      return res;
    }
    bool operator==(const bi_edge_supplier &other) const {
      return base_t::operator==(other) && data == other.data;
    }
    bool operator!=(const bi_edge_supplier &other) const {
      return !operator==(other);
    }
  };

 public:
  template<class EdgeIter, class NodeIter = empty_supplier>
  bipartite_graph(int n1, int n2, EdgeIter edge_begin, EdgeIter edge_end,
                  NodeIter node_begin = empty_supplier(0), NodeIter node_end = empty_supplier(0))
      : base(n1 + n2, bi_edge_supplier(edge_begin), bi_edge_supplier(edge_end), node_begin, node_end),
        n1(n1),
        n2(n2) {}

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

int main() {
  int n, m;
  std::cin >> n >> m;
  std::vector<std::vector<bool>> board(n);
  for (int i = 0; i < n; i++) {
    board[i].resize(m);
    for (int j = 0; j < m; j++) {
      char ch;
      std::cin >> ch;
      board[i][j] = ch == 'W';
    }
  }
  auto cell2diags = [&](int i, int j) -> std::pair<int, int> { return {i + j, i - j + m - 1}; };
  auto cellXOR00 = [&](int i, int j) -> bool { return (i + j) % 2; };
  auto diag2cell = [&](int diag) -> std::pair<int, int> {
    if (diag < n + m - 1) {
      if (diag < m - 1)
        return {0, diag};
      else
        return {diag - (m - 1), m - 1};
    } else {
      diag -= n + m - 1;
      if (diag < m - 1)
        return {0, m - 1 - diag};
      else
        return {diag - (m - 1), 0};
    }
  };

  bool correct_color_00 = false;
  std::vector<int> res_recolored_diags;
  bool res_color00 = false;
  do {
    // try each coloring by color of upper-left corner

    // build graph where vertices are diagonals and edges are wrongly colored cells
    // minimum vertex cover of g correspongs to a minimum set of recolored diagonals
    // as each two intersecting diagonals will be recolored the same color, order
    // of coloring is not important. Conversely, if we color two intersecting diagonals
    // different colors, it will lead to additional actions because in checkerboard all such
    // diagonals are the same color.
    std::vector<std::pair<int, int>> wrong_color;
    for (int i = 0; i < n; i++)
      for (int j = 0; j < m; j++)
        if (board[i][j] != (cellXOR00(i, j) ^ correct_color_00)) {
          auto[diag1, diag2] = cell2diags(i, j);
          wrong_color.push_back({diag1, diag2 + n + m - 1});
        }

    graph_t g(n + m - 1, n + m - 1, wrong_color.begin(), wrong_color.end());

    // find minimum vertex cover of g (corresponing minimum set of recolored diagonals)
    std::vector<int> recolored_diags;
    g.min_vertex_cover([&](int v) { recolored_diags.push_back(v); });
    if (!correct_color_00 || res_recolored_diags.size() > recolored_diags.size()) {
      // found new minimum
      res_recolored_diags = std::move(recolored_diags);
      res_color00 = correct_color_00;
    }

    correct_color_00 ^= 1;
  } while (correct_color_00);

  std::cout << res_recolored_diags.size() << std::endl;
  for (auto diag : res_recolored_diags) {
    auto[i, j] = diag2cell(diag);
    std::cout << (diag < n + m - 1 ? 1 : 2) << ' ' << i + 1 << ' ' << j + 1 << ' '
              << ((cellXOR00(i, j) ^ res_color00) ? 'W' : 'B') << std::endl;
  }

  return 0;
}
