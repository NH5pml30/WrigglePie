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
using graph_t = graph<empty, empty>;

graph_t read_graph(std::istream &in) {
  int n, m;
  if (!(in >> n >> m)) {
    edge_descriptor *it = nullptr;
    return graph_t(0, it, it);
  }

  using read_iterator = counting_istream_iterator<edge_descriptor>;

  return graph_t(n, read_iterator(in), read_iterator(m));
}

struct mask_t {
  uint64_t data;

  struct bit_proxy {
    friend struct mask_t;
   private:
    uint64_t &data;
    int index;

    bit_proxy(uint64_t &data, int index) : data(data), index(index) {}

   public:
    bit_proxy &operator=(bool val) noexcept {
      if (val)
        data |= (uint64_t{1} << index);
      else
        data &= ~(uint64_t{1} << index);
      return *this;
    }

    operator bool() const noexcept {
      return (data >> index) & 1;
    }
  };

  mask_t(uint64_t data = 0) noexcept : data(data) {}
  bool operator[](int index) const noexcept {
    return (data >> index) & 1;
  }

  mask_t operator|(mask_t other) const noexcept {
    return mask_t{data | other.data};
  }
  mask_t operator&(mask_t other) const noexcept {
    return mask_t{data & other.data};
  }
  mask_t &operator~() noexcept {
    data = ~data;
    return *this;
  }

  bit_proxy operator[](int index) noexcept {
    return bit_proxy(data, index);
  }

  int count() const noexcept {
    int res = 0;
    for (uint64_t ldata = data; ldata > 0; ldata >>= 1)
      res += (int)(ldata & 1);
    return res;
  }
};

mask_t get_neighbours(const graph_t &g, int v) {
  mask_t res;
  for (auto [other_id, data] : g.nodes[v].adjacent) {
    res[other_id] = true;
    data = data;
  }
  return res;
}

template<class Callback>
bool independent_sets_helper(graph_t &g, mask_t A, mask_t I, Callback &&callback) {
  if (A.data == 0) {
    return callback(I);
  }

  int min_intersection_s = std::numeric_limits<int>::max(), min_intersection_v = -1;
  for (int v = 0; v < g.size(); v++)
    if (A[v])
      if (int s = (get_neighbours(g, v) & A).count(); s < min_intersection_s)
        min_intersection_s = s, min_intersection_v = v;

  mask_t set =
      mask_t(uint64_t{1} << min_intersection_v) | (get_neighbours(g, min_intersection_v) & A);

  for (int u = 0; u < g.size(); u++)
    if (set[u]) {
      mask_t to_remove = (get_neighbours(g, u) & A) | mask_t{uint64_t{1} << u};
      if (independent_sets_helper(g, A & ~to_remove, I | mask_t{uint64_t{1} << u},
                                  std::forward<Callback>(callback)))
        return true;
    }
  return false;
}

template<class Callback>
void independent_sets(graph_t &g, Callback &&callback) {
  independent_sets_helper(g, (uint64_t{1} << g.size()) - 1, 0, std::forward<Callback>(callback));
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto &&in = std::cin;

  auto g = read_graph(in);

  std::vector<char> colors(g.size());

  independent_sets(g, [&](mask_t I) {
    // color independent set as 0
    for (int v = 0; v < g.size(); v++)
      if (I[v])
        colors[v] = 0;

    // try color with two colors what is left
    mask_t was;
    auto dfs = [&](int v, char color, auto &dfs) -> bool {
      was[v] = true;
      colors[v] = color;
      char next_color = (char)(3 - color);
      for (auto [other_id, data] : g.nodes[v].adjacent) {
        if (!I[other_id]) {
          if (!was[other_id]) {
            if (!dfs(other_id, next_color, dfs))
              return false;
          } else if (colors[other_id] != next_color) {
            return false;
          }
        }
        data = data;
      }
      return true;
    };
    for (int v = 0; v < g.size(); v++)
      if (!I[v] && !was[v] && !dfs(v, 1, dfs))
        return false;

    for (auto el : colors)
      std::cout << el + 1 << ' ';
    return true;
  });

  std::cout << std::endl;
  return 0;
}
