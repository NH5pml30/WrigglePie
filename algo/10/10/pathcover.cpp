/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <optional>
#include <numeric>
#include <algorithm>
#include <functional>
#include <cassert>
#include <unordered_set>
#include <iterator>
#include <set>

struct empty {};

template<class ChildT>
struct counting_iterator {
  using iterator_category = std::forward_iterator_tag;
  using value_type        = int;
  using difference_type   = int;
  using pointer           = int *;
  using reference         = int &;

  int cur = 0;

  counting_iterator(int begin = 0) : cur(begin) {}
  virtual void increment() {}
  virtual bool equals(const ChildT &other) const {
    return cur == static_cast<const counting_iterator &>(other).cur;
  }
  ChildT &operator++() {
    increment();
    cur++;
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
  bool operator==(const ChildT &other) const {
    return equals(other);
  }
  bool operator!=(const ChildT &other) const {
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
  using value_type = T;
  using pointer    = T *;
  using reference  = T &;

  supplier_iterator(int begin = 0) : base_t(begin) {}
  virtual T invoke() const {
    return T();
  }
  auto operator*() const {
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
      adjacent.emplace_back(other_id, std::move(data));
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
  struct node_iterator : base_graph_iterator<ChildT, GraphChildT, NODE_DATA> {
    using base_t = base_graph_iterator<ChildT, GraphChildT, NODE_DATA>;
    using base_t::base_graph_iterator;

    NODE_DATA invoke() const override {
      return base_t::enclosing->nodes[base_t::cur].data;
    }
  };

  template<class GraphChildT>
  struct node_iterator_default : node_iterator<node_iterator_default<GraphChildT>, GraphChildT> {
    using node_iterator<node_iterator_default, GraphChildT>::node_iterator;
  };

  static constexpr bool IS_EMPTY_EDGE = std::is_same_v<EDGE_DATA, empty>;
  using edge_descriptor_t =
      std::conditional_t<IS_EMPTY_EDGE, std::pair<int, int>, std::tuple<int, int, EDGE_DATA>>;
  static edge_descriptor_t create_edge_descriptor(int from, int to, EDGE_DATA data = {}) {
    if constexpr (IS_EMPTY_EDGE)
      return edge_descriptor_t(from, to);
    else
      return edge_descriptor_t(from, to, std::move(data));
  }
  static edge_descriptor_t create_edge_descriptor(int from, const typename NODE::EDGE_ENTRY &edge) {
    return create_edge_descriptor(from, edge.other_id, edge.data);
  }

  template<class ChildT, class GraphChildT>
  struct edge_iterator
      : base_graph_iterator<ChildT, GraphChildT, edge_descriptor_t> {
    using base_t = base_graph_iterator<ChildT, GraphChildT, edge_descriptor_t>;
    int from, off = -1;

    edge_iterator(GraphChildT *enclosing, int from) : base_t(enclosing), from(from) {
      increment();
    }
    edge_descriptor_t invoke() const override {
      return create_edge_descriptor(from, base_t::enclosing->nodes[from].adjacent[off].other_id);
    }
    void increment() override {
      off++;
      while (from < (int)base_t::enclosing->nodes.size() &&
             off >= (int)base_t::enclosing->nodes[from].adjacent.size()) {
        off = 0;
        from++;
      }
    }
    bool equals(const edge_iterator &other) const override {
      return from == other.from && off == other.off;
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
        auto[from, to, data] = *edge_begin;
        nodes[from].add_adjacent(to, std::move(data));
      }
      n_of_edges_++;
    }
  }

  graph reverse() const {
    struct reverse_edge_iterator : edge_iterator<reverse_edge_iterator, const graph> {
      using base_t = edge_iterator<reverse_edge_iterator, const graph>;

      using base_t::edge_iterator;
      edge_descriptor_t invoke() const override {
        auto &to = base_t::enclosing->nodes[base_t::from].adjacent[base_t::off];
        return create_edge_descriptor(to.other_id, base_t::from, to.data);
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

struct euler_flag_base {
  mutable bool used = false;

  void set_used(bool new_used) const {
    used = new_used;
  }
  bool get_used() const {
    return used;
  }
};

template<class NODE_DATA, class EDGE_DATA, class EdgeCallback>
std::enable_if_t<std::is_base_of_v<euler_flag_base, EDGE_DATA>>
    euler_(const graph<NODE_DATA, EDGE_DATA> &g, EdgeCallback &callback, int v) {
  for (auto &next : g.nodes[v].adjacent)
    if (!next.data.get_used()) {
      next.data.set_used(true);
      euler_(g, callback, next.other_id);
    }
  callback(v);
}

template<class NODE_DATA, class EDGE_DATA, class EdgeCallback>
std::enable_if_t<std::is_base_of_v<euler_flag_base, EDGE_DATA>>
    euler(const graph<NODE_DATA, EDGE_DATA> &g, EdgeCallback callback) {
  int start = 0;
  for (int v = 0; v < g.size(); v++) {
    for (auto &next : g.nodes[v].adjacent)
      next.data.set_used(false);
    if (g.nodes[v].adjacent.size() % 2 == 1)
      start = v;
  }

  euler_(g, callback, start);
}

using NODE_DATA_T = empty;

struct EDGE_DATA_T : euler_flag_base {
  EDGE_DATA_T *paired = nullptr;

  EDGE_DATA_T() = default;
  EDGE_DATA_T(EDGE_DATA_T *paired) : paired(paired) {
    if (paired != nullptr)
      paired->paired = this;
  }
  EDGE_DATA_T(EDGE_DATA_T &&other)
  noexcept : euler_flag_base(std::move(other)), paired(other.paired) {
    other.paired = nullptr;
    if (paired != nullptr)
      paired->paired = this;
  }
  EDGE_DATA_T & operator=(EDGE_DATA_T &&other) noexcept {
    euler_flag_base::operator=(std::move(other));
    paired = other.paired;
    other.paired = nullptr;
    if (paired != nullptr)
      paired->paired = this;
    return *this;
  }

  EDGE_DATA_T(const EDGE_DATA_T &other) = delete;
  EDGE_DATA_T & operator=(const EDGE_DATA_T &other) = delete;

  void set_used(bool new_used) const {
    euler_flag_base::set_used(new_used);
    if (paired != nullptr)
      paired->euler_flag_base::set_used(new_used);
  }
};

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

struct counting_istream_iterator : std::istream_iterator<int>,
                                    supplier_iterator<counting_istream_iterator, int> {
  using sup_base_t = supplier_iterator<counting_istream_iterator, int>;
  using input_base_t = std::istream_iterator<int>;

  counting_istream_iterator(std::istream &in, int begin = 0)
      : input_base_t(in), sup_base_t(begin) {}
  counting_istream_iterator(int begin = 0) : input_base_t(), sup_base_t(begin) {}
  int invoke() const override {
    return input_base_t::operator*();
  }
  void increment() override {
    input_base_t::operator++();
  }
  using sup_base_t::operator==;
  using sup_base_t::operator!=;
  using sup_base_t::operator*;
  using sup_base_t::operator++;
};

std::pair<graph_t, std::vector<int>> read_graph(std::istream &in) {
  int n, m;
  in >> n >> m;

  struct reader_iterator : supplier_iterator<reader_iterator, int> {
    using base_t = supplier_iterator<reader_iterator, int>;
    std::istream_iterator<int> cur;
    int m;
    std::vector<bool> &deg_parity;
    std::vector<int> &odd;
    std::vector<int> odd_buffer;

    enum class state {
      reading, evening_out, finished
    } stt = state::reading;

    reader_iterator(std::istream &in, int m, std::vector<bool> &deg_parity, std::vector<int> &odd)
        : base_t(), cur(in), m(m), deg_parity(deg_parity), odd(odd) {
      if (m > 0) {
        deg_parity[*cur - 1].flip();
      }
    }
    reader_iterator(int m, std::vector<bool> &deg_parity, std::vector<int> &odd)
        : base_t(m), cur(), m(m), deg_parity(deg_parity), odd(odd), stt(state::finished) {
    }

    int invoke() const override {
      switch (stt) {
      case state::reading:
        return *cur;
      case state::evening_out:
        return odd.back() + 1;
      default:
        return -1;
      }
    }
    void increment() override {
      if (base_t::cur < m - 1) {
        cur++;
        deg_parity[*cur - 1].flip();
      } else {
        stt = state::evening_out;
        if (base_t::cur == m - 1) {
          for (int i = 0; i < (int)deg_parity.size(); i++)
            if (deg_parity[i])
              odd_buffer.push_back(i);
          // leave two odd vertices, if can
          if (odd_buffer.size() >= 2) {
            odd_buffer.pop_back();
            odd_buffer.pop_back();
          }
         }
        if (!odd_buffer.empty()) {
          odd.push_back(odd_buffer.back());
          odd_buffer.pop_back();
        } else {
          stt = state::finished;
        }
      }
    }
    bool equals(const reader_iterator &other) const override {
      return stt == other.stt &&
             ((stt == state::reading && base_t::operator==(other)) ||
              (stt == state::evening_out && deg_parity.size() == other.deg_parity.size()) ||
              stt == state::finished);
    }
  };

  struct edge_inner_supplier : supplier_iterator<edge_inner_supplier, std::pair<int, int>> {
    reader_iterator cur, end;
    int m;
    int from, to;

    edge_inner_supplier(std::istream &in, std::vector<bool> &deg_parity, std::vector<int> &odd,
                        std::vector<bool> &b_empty, std::vector<int> &i_empty, int m)
        : cur(in, m * 2, deg_parity, odd), end(m * 2, b_empty, i_empty), m(m) {
      read();
    }
    edge_inner_supplier(std::vector<bool> &b_empty, std::vector<int> &i_empty, int m)
        : cur(m * 2, b_empty, i_empty), end(m * 2, b_empty, i_empty), m(m) {}

    void read() {
      static bool is_first = true;
      if (is_first)
        is_first = false;
      else
        ++cur;
      from = *cur - 1;
      to = *++cur - 1;
    }
    std::pair<int, int> invoke() const override {
      return {from, to};
    }
    void increment() override {
      if (cur == end)
        return;
      read();
    }
    bool equals(const edge_inner_supplier &other) const override {
      return cur == other.cur;
    }
  };

  struct edge_outer_supplier : supplier_iterator<edge_outer_supplier, graph_t::edge_descriptor_t> {
    edge_inner_supplier data;
    bool is_reverse = false;
    mutable EDGE_DATA_T tracker;

    edge_outer_supplier(std::istream &in, std::vector<bool> &deg_parity, std::vector<int> &odd,
                        std::vector<bool> &b_empty, std::vector<int> &i_empty, int m)
        : data(in, deg_parity, odd, b_empty, i_empty, m) {}
    edge_outer_supplier(std::vector<bool> &b_empty, std::vector<int> &i_empty, int m)
        : data(b_empty, i_empty, m) {}

    graph_t::edge_descriptor_t invoke() const override {
      auto[from, to] = data.invoke();
      if (!is_reverse) {
        return graph_t::create_edge_descriptor(from, to, EDGE_DATA_T(&tracker));
      } else {
        return graph_t::create_edge_descriptor(to, from, EDGE_DATA_T(tracker.paired));
      }
    }
    void increment() override {
      if (is_reverse)
        ++data;
      is_reverse ^= 1;
    }
    bool equals(const edge_outer_supplier &other) const override {
      return data == other.data && is_reverse == other.is_reverse;
    }
  };

  std::vector<bool> deg_parity(n), b_empty;
  std::vector<int> odd, i_empty;

  graph_t g = graph_t(n, edge_outer_supplier(in, deg_parity, odd, b_empty, i_empty, m),
                      edge_outer_supplier(b_empty, i_empty, m));
  return {std::move(g), odd};
}

int main() {
  auto[g, odd] = read_graph(std::cin);
  std::set<std::pair<int, int>> added_edges;
  for (int i = 0; i < (int)odd.size(); i += 2)
    added_edges.insert(std::minmax(odd[i], odd[i + 1]));
  std::cout << (int)odd.size() / 2 + 1 << std::endl;

  euler(g, [&, last_v = -1](int v) mutable {
    if (last_v != -1) {
      if (auto it = added_edges.find(std::minmax(last_v, v)); it != added_edges.end()) {
        std::cout << '\n';
        added_edges.erase(it);
      }
    }
    std::cout << v + 1 << ' ';
    last_v = v;
  });
  std::cout << std::endl;
  return 0;
}
