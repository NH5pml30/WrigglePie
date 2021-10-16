/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <cassert>
#include <stack>
#include <optional>
#include <variant>
#include <sstream>
#include <random>
#include <bitset>
#include <list>
#include <iterator>

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

template<class T>
struct supplier_iterator_t : supplier_iterator<supplier_iterator_t<T>, T> {
  using supplier_iterator<supplier_iterator_t, T>::supplier_iterator;
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
        nodes[to].add_adjacent(from, {});
      } else {
        auto &&[from, to, data] = *edge_begin;
        nodes[from].add_adjacent(to, data);
        nodes[to].add_adjacent(from, data);
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

template<typename... First>
struct readable_tuple : std::tuple<First...> {
  using std::tuple<First...>::tuple;

  template<size_t ind = 0>
  static void read(std::istream &in, std::tuple<First...> &tuple) {
    in >> std::get<ind>(tuple);
    --std::get<ind>(tuple);
    if constexpr (ind + 1 != sizeof...(First))
      read<ind + 1>(in, tuple);
  }

  friend std::istream &operator>>(std::istream &in, readable_tuple &tuple) {
    read(in, tuple);
    return in;
  }
};

namespace std {
  template<typename... First>
  struct tuple_size<readable_tuple<First...>> : tuple_size<std::tuple<First...>> {
  };

  template<size_t i, typename... First>
  struct tuple_element<i, readable_tuple<First...>> : tuple_element<i, std::tuple<First...>> {
  };
}

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

using edge_descriptor = readable_tuple<int, int>;
using graph_t = graph<int, empty>;

std::pair<graph_t, int> read_graph(std::istream &in) {
  int n, m, k;
  if (!(in >> n >> m >> k)) {
    edge_descriptor *it = nullptr;
    return {graph_t(0, it, it, supplier_iterator_t<int>(0), supplier_iterator_t<int>(0)), k};
  }

  using read_iterator = counting_istream_iterator<edge_descriptor>;

  return {graph_t(n, read_iterator(in), read_iterator(m), supplier_iterator_t<int>(0),
                  supplier_iterator_t<int>(n)),
          k};
}

std::vector<bool> get_neighbours(const graph_t &g, int v) {
  std::vector<bool> res(g.nodes.size());
  for (auto [other_id, data] : g.nodes[v].adjacent) {
    res[other_id] = true;
    data = data;
  }
  return res;
}

std::vector<bool> &operator&=(std::vector<bool> &lhs, const std::vector<bool> &rhs) {
  assert(lhs.size() == rhs.size());
  std::transform(lhs.begin(), lhs.end(), rhs.begin(), lhs.begin(),
                 std::logical_and<>());
  return lhs;
}
std::vector<bool> &&operator&=(std::vector<bool> &&lhs, const std::vector<bool> &rhs) {
  return std::move(lhs &= rhs);
}

std::vector<bool> &operator|=(std::vector<bool> &lhs, const std::vector<bool> &rhs) {
  assert(lhs.size() == rhs.size());
  std::transform(lhs.begin(), lhs.end(), rhs.begin(), lhs.begin(),
                 std::logical_or<>());
  return lhs;
}
std::vector<bool> &&operator|=(std::vector<bool> &&lhs, const std::vector<bool> &rhs) {
  return std::move(lhs |= rhs);
}

std::vector<bool> &negate(std::vector<bool> &lhs) {
  std::transform(lhs.begin(), lhs.end(), lhs.begin(),
                 std::logical_not<>());
  return lhs;
}
std::vector<bool> &&negate(std::vector<bool> &&lhs) {
  return std::move(negate(lhs));
}

int count(const std::vector<bool> &v) {
  return (int)std::count(v.begin(), v.end(), true);
}

void vertex_cover_helper(graph_t &g, int k, std::vector<bool> &A, std::vector<bool> &C,
                         std::vector<bool> &not_C, int n_in_cover, int n_covered,
                         std::vector<bool> &result) {
  if (n_in_cover > k || !result.empty()) {
    return;
  }

  if (n_covered == g.n_of_edges()) {
    result = C;
    return;
  }

  int max_intersection_s = -1, max_intersection_v = -1;
  for (int v = 0; v < (int)g.nodes.size(); v++)
    if (A[v] && g[v] > max_intersection_s)
      max_intersection_s = g[v], max_intersection_v = v;

  auto a_copy = A, c_copy = C;

  if (max_intersection_s <= 2) {
    auto process_path_or_cycle = [&](int v, int parity) {
      int last = -1, counter = parity;
      while (n_in_cover <= k) {
        A[v] = false;
        if (counter++ % 2 == 0) {
          C[v] = true;
          n_in_cover++;
        }
        if (auto iter = std::find_if(
                g.nodes[v].adjacent.begin(), g.nodes[v].adjacent.end(),
                [&](auto &edge) { return A[edge.other_id] && edge.other_id != last; });
            iter != g.nodes[v].adjacent.end()) {
          v = iter->other_id;
        } else {
          break;
        }
      }
    };

    // cycles and paths
    for (int v = 0; v < g.size() && n_in_cover <= k; v++)
      if (A[v] && g[v] == 1)
        // beginning of a path
        process_path_or_cycle(v, 1);
    // only cycles left
    for (int v = 0; v < g.size() && n_in_cover <= k; v++)
      if (A[v] && g[v] == 2)
        // inside a cycle
        process_path_or_cycle(v, 0);
    vertex_cover_helper(g, k, A, C, not_C, n_in_cover, g.n_of_edges(), result);
    A = a_copy, C = c_copy;
    not_C = negate(std::move(c_copy));
    return;
  }

  if (max_intersection_s <= 1) {
    for (int v = 0; v < g.size() && n_in_cover <= k; v++)
      if (A[v]) {
        A[v] = false;
        if (auto iter = std::find_if(g.nodes[v].adjacent.begin(), g.nodes[v].adjacent.end(),
                                     [&](auto &edge) { return A[edge.other_id]; });
            iter != g.nodes[v].adjacent.end()) {
          C[v] = true;
          A[iter->other_id] = false;
          n_in_cover++;
        }
      }
    vertex_cover_helper(g, k, A, C, not_C, n_in_cover, g.n_of_edges(), result);
    A = a_copy, C = c_copy;
    not_C = negate(std::move(c_copy));
    return;
  }

  auto add_to_cover = [&](int v) {
    assert(A[v]);
    A[v] = false;
    C[v] = true;
    not_C[v] = false;
    for (auto &[other_id, data] : g.nodes[v].adjacent) {
      g[other_id]--;
      data = data;
    }
  };

  auto remove_from_cover = [&](int v) {
    C[v] = false;
    not_C[v] = true;
  };

  auto revert_degree = [&](int v) {
    for (auto &[other_id, data] : g.nodes[v].adjacent) {
      g[other_id]++;
      data = data;
    }
  };

  // choose v
  add_to_cover(max_intersection_v);
  vertex_cover_helper(g, k, A, C, not_C, n_in_cover + 1,
                      n_covered + count(get_neighbours(g, max_intersection_v) &= not_C), result);
  remove_from_cover(max_intersection_v);

  // do not choose v, choose neighbours
  int new_n_covered = n_covered;
  for (auto &[other_id, data] : g.nodes[max_intersection_v].adjacent)
    if (A[other_id]) {
      add_to_cover(other_id);
      for (auto &[other_other_id, data] : g.nodes[other_id].adjacent) {
        new_n_covered += !C[other_other_id];
        data = data;
      }
      data = data;
    }
  vertex_cover_helper(g, k, A, C, not_C, n_in_cover + max_intersection_s, new_n_covered, result);

  A = a_copy, C = c_copy;
  not_C = negate(std::move(c_copy));
  revert_degree(max_intersection_v);
  for (auto &[other_id, data] : g.nodes[max_intersection_v].adjacent)
    if (A[other_id]) {
      revert_degree(other_id);
      data = data;
    }
}

std::vector<bool> vertex_cover(graph_t &g, int k) {
  for (int v = 0; v < (int)g.size(); v++)
    g[v] = (int)g.nodes[v].adjacent.size();

  std::vector<bool> A(g.nodes.size(), true), C(g.nodes.size()), not_C(g.nodes.size(), true), result;
  vertex_cover_helper(g, k, A, C, not_C, 0, 0, result);
  return result;
}

namespace ideal {
  void vertex_cover_helper(const graph_t &g, int k, std::vector<bool> &A, std::vector<bool> &C, std::vector<bool> &result) {
    if (count(C) > k) {
      return;
    }

    auto check_covered = [&]() {
      int n_covered = 0;
      for (int v = 0; v < (int)g.nodes.size(); v++)
        if (C[v])
          for (auto &[other_id, data] : g.nodes[v].adjacent) {
            n_covered += !C[other_id] || (C[other_id] && other_id < v);
            [[maybe_unused]] auto d = data;
          }
      return n_covered == g.n_of_edges();
    };

    if (check_covered()) {
      result = C;
      return;
    }

    std::vector<bool> max_intersection;
    int max_intersection_s = -1, max_intersection_v = -1;
    for (int v = 0; v < (int)g.nodes.size(); v++)
      if (A[v]) {
        auto intersection = get_neighbours(g, v) &= A;
        int intersection_s = count(intersection);
        if (intersection_s > max_intersection_s)
          max_intersection.swap(intersection), max_intersection_s = intersection_s, max_intersection_v = v;
      }

    if (max_intersection_s == -1)
      return;

    auto a_copy = A, c_copy = C;

    if (max_intersection_s <= 1) {
      int cs = count(C);
      for (int v = 0; v < (int)g.nodes.size() && cs <= k; v++)
        if (A[v]) {
          A[v] = false;
          if (auto iter = std::find_if(g.nodes[v].adjacent.begin(), g.nodes[v].adjacent.end(),
                                       [&](auto &edge) { return A[edge.other_id]; });
              iter != g.nodes[v].adjacent.end() && !C[iter->other_id]) {
            C[v] = true;
            cs++;
            A[iter->other_id] = false;
          }
        }
      vertex_cover_helper(g, k, A, C, result);
      A = a_copy, C = c_copy;
      return;
    }

    // choose v
    A[max_intersection_v] = false;
    C[max_intersection_v] = true;
    vertex_cover_helper(g, k, A, C, result);

    // do not choose v, choose neighbours
    C |= max_intersection;
    C[max_intersection_v] = false;
    A &= negate(max_intersection);
    vertex_cover_helper(g, k, A, C, result);

    A = a_copy, C = c_copy;
  }

  std::vector<bool> vertex_cover(const graph_t &g, int k) {
    std::vector<bool> A(g.nodes.size(), true), I(g.nodes.size()), result;
    vertex_cover_helper(g, k, A, I, result);
    return result;
  }
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto &&in = std::cin;

  auto [g, k] = read_graph(in);

  auto cover = vertex_cover(g, k);
  if (!cover.empty()) {
    int diff = k - count(cover);
    std::cout << "Yes\n";
    for (int i = 0; i < (int)g.nodes.size(); i++)
      if (diff > 0 || cover[i]) {
        std::cout << i + 1 << ' ';
        diff -= !cover[i];
      }
    std::cout << std::endl;
  } else {
    std::cout << "No" << std::endl;
  }

  return 0;
}
