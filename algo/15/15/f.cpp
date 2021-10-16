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
#include <ctime>

namespace ideal {
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

  struct edge_data_t : pair_edge_base<edge_data_t> {
    int id, color = -1;
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

  struct edge_outer_supplier : supplier_iterator<edge_outer_supplier, const edge_descriptor> {
    using istream_iterator = counting_istream_iterator<readable_tuple<int, int>>;
    mutable istream_iterator cur;
    mutable int counter = 0;
    mutable edge_data_t tracker;
    mutable std::optional<edge_descriptor> data;

    edge_outer_supplier(std::istream &in)
        : cur(in) {}
    edge_outer_supplier(int m)
        : cur(m) {}

    const edge_descriptor &invoke() const override {
      if (!data.has_value()) {
        switch (counter) {
        case 0:
          data = edge_descriptor{std::get<0>(*cur) - 1, std::get<1>(*cur) - 1,
                                 edge_data_t{&tracker, cur.cur}};
          break;
        case 1:
          data = edge_descriptor{std::get<1>(*cur) - 1, std::get<0>(*cur) - 1,
                                 edge_data_t{tracker.paired, cur.cur}};
          break;
        }
      }
      return *data;
    }
    void increment() override {
      counter++;
      if (counter == 2) {
        counter = 0;
        ++cur;
      }
      data.reset();
    }
    bool equals(const edge_outer_supplier &other) const override {
      return cur == other.cur;
    }
  };

  graph_t read_graph(std::istream &in) {
    int n, m;
    if (!(in >> n >> m)) {
      edge_descriptor *it = nullptr;
      return graph_t(0, it, it);
    }

    return graph_t(n, edge_outer_supplier(in), edge_outer_supplier(m));
  }

  void color_edges_helper(graph_t &g, std::list<int> &queue,
                          std::vector<std::list<int>::iterator> &queue_place, bool &is_res_found) {
    if (is_res_found)
      return;

    if (queue.empty()) {
      is_res_found = true;
      return;
    }

    int v = queue.front();
    queue.pop_front();

    auto get_edge_colors = [&](int v) {
      std::set<int> colors;
      for (auto &edge : g.nodes[v].adjacent)
        if (edge.data.color != -1)
          colors.insert(edge.data.color);
      return colors;
    };

    auto check_validity = [&](int v) {
      std::set<int> colors;
      for (auto &edge : g.nodes[v].adjacent)
        if (edge.data.color != -1) {
          if (colors.count(edge.data.color))
            return false;
          colors.insert(edge.data.color);
        }
      return true;
    };

    auto invert_colors = [](const std::set<int> &colors) {
      std::set<int> res;
      integer_set_complement(3, colors.begin(), colors.end(), std::inserter(res, res.end()), [](int x) { return x; });
      return res;
    };

    auto edge_coloring_changed = [&](int node) {
      if (queue_place[node] == queue.end()) {
        queue.push_back(node);
        queue_place[node] = --queue.end();
      }
    };

    auto uncolor_edge = [&](auto &edge) {
      edge.data.color = -1;
      edge.data.paired->color = -1;
      edge_coloring_changed(edge.other_id);
    };

    auto color_edge = [&](auto &edge, int color) {
      edge.data.color = color;
      edge.data.paired->color = color;
      if (check_validity(edge.other_id)) {
        edge_coloring_changed(edge.other_id);
        return true;
      }
      edge.data.color = -1;
      edge.data.paired->color = -1;
      return false;
    };

    auto colors = invert_colors(get_edge_colors(v)), colors_copy = colors;
    bool recursed = false;
    for (int color : colors) {
      colors_copy.erase(color);

      for (auto &edge : g.nodes[v].adjacent)
        if (edge.data.color == -1) {
          recursed = true;
          if (color_edge(edge, color)) {
            bool recursed_2 = false;
            if (!colors_copy.empty()) {
              for (auto &edge_2 : g.nodes[v].adjacent)
                if (edge_2.other_id != edge.other_id && edge_2.data.color == -1) {
                  recursed_2 = true;
                  if (color_edge(edge_2, *colors_copy.begin())) {
                    color_edges_helper(g, queue, queue_place, is_res_found);
                    if (is_res_found)
                      return;
                    uncolor_edge(edge_2);
                    break;
                  }
                }
            }
            if (!recursed_2) {
              color_edges_helper(g, queue, queue_place, is_res_found);
              if (is_res_found)
                return;
            }
            uncolor_edge(edge);
          }
        }

      colors_copy.insert(color);
    }
    if (!recursed) {
      color_edges_helper(g, queue, queue_place, is_res_found);
      if (is_res_found)
        return;
    }

    queue.push_front(v);
    queue_place[v] = queue.begin();
  }

  bool color_edges(graph_t &g) {
    if (!std::all_of(g.nodes.begin(), g.nodes.end(),
                     [](auto &node) { return node.adjacent.size() <= 3; })) {
      return false;
    }

    std::vector<bool> was(g.size());
    std::vector<int> component_representatives;

    auto dfs = [&](int v, auto &dfs) -> void {
      was[v] = true;
      for (auto &edge : g.nodes[v].adjacent)
        if (!was[edge.other_id])
          dfs(edge.other_id, dfs);
    };

    for (int v = 0; v < g.size(); v++)
      if (!was[v]) {
        component_representatives.push_back(v);
        dfs(v, dfs);
      }

    for (int v : component_representatives) {
      if (!g.nodes[v].adjacent.empty()) {
        auto &edge = g.nodes[v].adjacent.front();
        edge.data.color = 0;
        edge.data.paired->color = 0;

        std::list<int> queue;
        queue.push_back(v);
        queue.push_back(edge.other_id);

        std::vector<std::list<int>::iterator> queue_place(g.size(), queue.end());
        bool is_res_found = false;
        color_edges_helper(g, queue, queue_place, is_res_found);
        if (!is_res_found)
          return false;
      }
    }
    return true;
  }
}

using graph_t = std::vector<std::array<int, 3>>;

bool check_coloring(graph_t &g, std::vector<int> &colors_data) {
  for (int v = 0; v < (int)g.size(); v++) {
    int colors = 0;
    for (int u : g[v])
      if (u != -1) {
        if (colors_data[v * g.size() + u] == -1) {
          std::cout << "-1" << std::endl;
          return false;
        }
        if ((colors >> colors_data[v * g.size() + u]) & 1) {
          std::cout << "repeat" << std::endl;
          return false;
        }
        colors |= 1 << colors_data[v * g.size() + u];
      }
  }
  return true;
}

int get_edge_colors(graph_t &g, int v, std::vector<int> &colors_data) {
  int colors = 0;
  for (int u : g[v])
    if (u != -1)
      if (auto col = colors_data[v * g.size() + u]; col != -1)
        colors |= 1 << col;
  return colors;
}

bool check_validity(graph_t &g, int v, std::vector<int> &colors_data) {
  int colors = 0;
  for (int u : g[v])
    if (u != -1)
      if (int col = colors_data[v * g.size() + u]; col != -1) {
        if ((colors >> col) & 1)
          return false;
        colors |= 1 << col;
      }
  return true;
}

int invert_colors(int colors) {
  return ~colors & 0b111;
}

void change_undef_deg(int v, int diff, std::list<std::pair<int, int>> &queue,
                      std::vector<int> &undef_deg,
                      std::vector<std::list<std::pair<int, int>>::iterator> &places) {
  if (undef_deg[v] < 3 && undef_deg[v] + diff == 3) {
    queue.erase(places[v]);
    places[v] = queue.end();
  }
  if (undef_deg[v] == 3 && undef_deg[v] + diff < 3) {
    queue.push_back({v, 0});
    places[v] = --queue.end();
  }
  undef_deg[v] += diff;
}

void uncolor_edge(graph_t &g, int v, int u, std::list<std::pair<int, int>> &queue,
                  std::vector<int> &undef_deg,
                  std::vector<std::list<std::pair<int, int>>::iterator> &places,
                  std::vector<int> &colors_data) {
  int old_color = colors_data[v * g.size() + u];
  colors_data[v * g.size() + u] = colors_data[u * g.size() + v] = -1;
  change_undef_deg(u, 1, queue, undef_deg, places);
  if (places[u] != queue.end())
    places[u]->second &= ~(1 << old_color);
}

bool color_edge(graph_t &g, int v, int u, int color, std::list<std::pair<int, int>> &queue,
                std::vector<int> &undef_deg,
                std::vector<std::list<std::pair<int, int>>::iterator> &places,
                std::vector<int> &colors_data) {
  colors_data[v * g.size() + u] = colors_data[u * g.size() + v] = color;

  if (check_validity(g, u, colors_data)) {
    change_undef_deg(u, -1, queue, undef_deg, places);
    if (places[u] != queue.end())
      places[u]->second |= 1 << color;
    return true;
  }
  colors_data[v * g.size() + u] = colors_data[u * g.size() + v] = -1;
  return false;
}

int start = clock();

bool color_edges_helper(graph_t &g, int component_mask,
                        std::list<std::pair<int, int>> &queue,
                        std::vector<int> &undef_deg,
                        std::vector<std::list<std::pair<int, int>>::iterator> &places,
                        std::vector<int> &colors_data) {
  if (clock() - start > 0.99 * CLOCKS_PER_SEC)
    return false;
  if (queue.empty()) {
    return true;
  }

  auto [v, cols] = queue.front();
  queue.pop_front();
  places[v] = queue.end();

  int colors[2] = {-1, -1};
  int pos = 0;
  for (int i = 0; i < 3; i++)
    if (((cols >> i) & 1) == 0)
      colors[pos++] = i;

  int next[2] = {-1, -1};
  pos = 0;
  for (int u : g[v])
    if (u != -1 && colors_data[v * g.size() + u] == -1)
      next[pos++] = u;

  auto try_coloring = [&](int v, int parity) -> bool {
    int saved = undef_deg[v];
    undef_deg[v] = 0;
    if (next[0] != -1) {
      if (colors[parity] != -1 &&
          color_edge(g, v, next[0], colors[parity], queue, undef_deg, places, colors_data)) {
        if (next[1] != -1) {
          if (colors[parity ^ 1] != -1 && color_edge(g, v, next[1], colors[parity ^ 1], queue,
                                                     undef_deg, places, colors_data)) {
            if (color_edges_helper(g, component_mask, queue, undef_deg, places, colors_data))
              return true;
            uncolor_edge(g, v, next[1], queue, undef_deg, places, colors_data);
          }
        } else {
          if (color_edges_helper(g, component_mask, queue, undef_deg, places, colors_data))
            return true;
        }
        uncolor_edge(g, v, next[0], queue, undef_deg, places, colors_data);
      }
    } else {
      if (color_edges_helper(g, component_mask, queue, undef_deg, places, colors_data))
        return true;
    }
    undef_deg[v] = saved;
    return false;
  };

  if (try_coloring(v, 0) || try_coloring(v, 1))
    return true;
  queue.push_front({v, cols});
  places[v] = queue.begin();
  return false;
}

std::vector<int> color_edges(graph_t &g) {
  std::vector<bool> was(g.size());
  std::vector<int> component_representatives, component_masks;

  auto dfs = [&](int v, int &mask, auto &dfs) -> void {
    was[v] = true;
    mask |= 1 << v;
    for (int u : g[v])
      if (u != -1 && !was[u])
        dfs(u, mask, dfs);
  };

  for (int v = 0; v < (int)g.size(); v++)
    if (!was[v]) {
      component_representatives.push_back(v);
      int mask = 0;
      dfs(v, mask, dfs);
      component_masks.push_back(mask);
    }

  std::vector<int> colors_data(g.size() * g.size(), -1);
  std::list<std::pair<int, int>> queue;
  std::vector<int> undef_deg(g.size());
  std::vector<std::list<std::pair<int, int>>::iterator> places(g.size());

  for (int vi = 0; vi < (int)component_representatives.size(); vi++) {
    int v = component_representatives[vi];

    std::fill(undef_deg.begin(), undef_deg.end(), 3);
    queue.clear();
    std::fill(places.begin(), places.end(), queue.end());

    int color = 0;
    for (int u : g[v])
      if (u != -1) {
        colors_data[v * g.size() + u] = colors_data[u * g.size() + v] = color;
        undef_deg[u] = 2;
        queue.push_back({u, 1 << color++});
        places[u] = --queue.end();
      }
    undef_deg[v] = 0;

    if (!color_edges_helper(g, component_masks[vi], queue, undef_deg, places, colors_data))
      return {};
  }
  return colors_data;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto &&in = std::cin;

#if 1

  int n, m;
  in >> n >> m;

  graph_t g(n, {-1, -1, -1});
  std::vector<int> degs(n);
  std::vector<int> id2packed;

  for (int i = 0; i < m; i++) {
    int u, v;
    in >> u >> v;
    u--, v--;
    if (degs[u] == 3 || degs[v] == 3) {
      std::cout << "NO" << std::endl;
      return 0;
    }
    g[u][degs[u]++] = v;
    g[v][degs[v]++] = u;
    id2packed.push_back(u * n + v);
  }

#else

  int n = 25;
  ideal::edge_descriptor *it = nullptr;
  ideal::graph_t gg(n, it, it);
  graph_t g(n, {-1, -1, -1});
  std::vector<int> degs(n);
  std::vector<int> id2packed;
  int id = 0;
  srand(time(nullptr));
  for (int c = 0; c < 500; c++) {
    int i = rand() % n, j = rand() % n;
    if (i != j && degs[i] < 3 && degs[j] < 3 &&
        std::find(g[i].begin(), g[i].end(), j) == g[i].end()) {
      g[i][degs[i]++] = j;
      g[j][degs[j]++] = i;
      id2packed.push_back(i * n + j);
      gg.nodes[i].add_adjacent(j, {nullptr, (int)id2packed.size() - 1});
      gg.nodes[j].add_adjacent(i, {&gg.nodes[i].adjacent.back().data, (int)id2packed.size() - 1});
    }
  }

#endif

  if (auto colors = color_edges(g); !colors.empty()) {
    // if (!check_coloring(g, colors)) {
    //   std::cout << "FAILED\n";
    //   return 0;
    // }
    std::cout << "YES\n";
    for (auto packed : id2packed)
      std::cout << colors[packed] + 1 << ' ';
    std::cout << std::endl;
  } else {
    // if (ideal::color_edges(gg)) {
    //   std::cout << "FAILED 1\n";
    //   return 0;
    // }
    std::cout << "NO" << std::endl;
  }

  return 0;
}
