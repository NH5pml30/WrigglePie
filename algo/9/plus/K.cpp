/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <functional>
#include <iostream>
#include <fstream>
#include <limits>
#include <map>
#include <numeric>
#include <queue>
#include <tuple>
#include <vector>
#include <optional>
#include <cassert>
#include <unordered_set>

template<typename DATA>
class disjoint_set {
 private:
  struct NODE_DATA {
    int index, parent, rank;
    DATA data;

    NODE_DATA(int index, DATA data) : index(index), parent(index), rank(0), data(std::move(data))
    {}

    NODE_DATA() {}

    static void attach(NODE_DATA &X, NODE_DATA &Y) {
      if (X.rank > Y.rank) {
        Y.parent = X.index;
        X.data = std::move(Y.data);
      } else {
        X.parent = Y.index;
        Y.data = std::move(X.data);
        if (X.rank == Y.rank)
          Y.rank++;
      }
    }
  };

  std::vector<NODE_DATA> nodes;

 public:
  disjoint_set(int n) : nodes(n) {
    int i = 0;
    for (auto &el : nodes) el = NODE_DATA(i++, {});
  }

  int find(int x) {
    if (nodes[x].parent == x)
      return x;
    return nodes[x].parent = find(nodes[x].parent);
  }

  DATA & operator[](int x) {
    return nodes[find(x)].data;
  }

  void join(int x, int y) {
    x = find(x), y = find(y);
    if (x == y)
      return;

    NODE_DATA::attach(nodes[x], nodes[y]);
  }
};

template<class T, class RhsT = T, class OpTRhs = std::plus<>, class OpRhsRhs = std::plus<>>
class skew_heap {
 private:
  OpTRhs op_applier;
  OpRhsRhs op_combiner;

  struct NODE {
    T data;
    RhsT cached_op{};
    NODE *left = nullptr, *right = nullptr;

    NODE(T data) : data(std::move(data)) {
    }

    void apply_op(const OpRhsRhs &op_combiner, const RhsT &rhs) {
      cached_op = op_combiner(cached_op, rhs);
    }

    void propagate(const OpTRhs &op_applier, const OpRhsRhs &op_combiner) {
      data = op_applier(data, cached_op);
      if (left != nullptr)
        left->apply_op(op_combiner, cached_op);
      if (right != nullptr)
        right->apply_op(op_combiner, cached_op);
      cached_op = RhsT();
    }

    static NODE * merge(NODE *a, NODE *b, const skew_heap *a_h, const skew_heap *b_h) {
      if (a == nullptr)
        return b;
      if (b == nullptr)
        return a;
      b->propagate(b_h->op_applier, b_h->op_combiner);
      a->propagate(a_h->op_applier, a_h->op_combiner);
      if (b->data < a->data) {
        std::swap(a, b);
        std::swap(a_h, b_h);
      }
      a->right = merge(a->right, b, a_h, b_h);
      std::swap(a->left, a->right);
      return a;
    }

    ~NODE() {
      delete left;
      delete right;
    }
  };

  NODE *root = nullptr;

 public:
  skew_heap(OpTRhs applier = OpTRhs(), OpRhsRhs combiner = OpRhsRhs())
      : op_applier(std::move(applier)), op_combiner(std::move(combiner)) {}

  skew_heap(skew_heap &&other)
      : root(other.root),
        op_applier(std::move(other.op_applier)),
        op_combiner(std::move(other.op_combiner)) {
    other.root = nullptr;
  }

  void insert(T key) {
    root = NODE::merge(root, new NODE(std::move(key)), this, this);
  }

  skew_heap & operator=(skew_heap &&other) noexcept {
    if (this != &other) {
      root = other.root;
      op_applier = std::move(other.op_applier);
      op_combiner = std::move(other.op_combiner);
      other.root = nullptr;
    }
    return *this;
  }

  T extract_min() {
    assert(root != nullptr);

    root->propagate(op_applier, op_combiner);
    T result = std::move(root->data);
    NODE *saved = root;
    root = NODE::merge(root->left, root->right, this, this);
    saved->left = saved->right = nullptr;
    delete saved;
    return std::move(result);
  }

  T top() const {
    return root->data;
  }

  void apply_operation(const RhsT &rhs) {
    if (root != nullptr)
      root->apply_op(op_combiner, rhs);
  }

  void merge(skew_heap &&other) {
    if (this == &other)
      return;
    root = NODE::merge(root, other.root, this, &other);
    other.root = nullptr;
  }

  bool empty() {
    return root == nullptr;
  }

  ~skew_heap() {
    delete root;
  }
};

template<typename T = int>
constexpr T infinity = std::numeric_limits<T>::max();

template<typename T = int>
constexpr T neg_infinity = std::numeric_limits<T>::min();

struct empty {};

struct noop_t {
  void operator()(...) const {}
};
noop_t noop;

template<class NODE_DATA = empty, class EDGE_DATA = empty>
class graph {
 private:
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

    void add_adjacent(int other_id, const EDGE_DATA &data) {
      adjacent.push_back(EDGE_ENTRY(other_id, data));
    }
  };
  std::vector<NODE> nodes;
  size_t n_of_edges = 0;

 public:
  enum class color { white, gray, black };
  std::vector<color> cols;

  class NODE_PROXY {
    friend class EDGE_PROXY;

   public:
    int id;
    NODE_DATA *data;

    bool is_valid() const {
      return data != nullptr;
    }

    NODE_PROXY(graph &g, int id) : id(id), data(id >= 0 ? &g.nodes[id].data : nullptr) {}
  };

  class EDGE_PROXY {
    friend class graph;

   public:
    NODE_PROXY from, to;
    EDGE_DATA *edge_data;

    EDGE_PROXY(graph &g, int from, typename NODE::EDGE_ENTRY &entry)
        : from(g, from), to(g, entry.other_id), edge_data(&entry.data) {}

    EDGE_PROXY(graph &g, int to) : from(g, -1), to(g, to), edge_data(nullptr) {}
  };

 private:
  template<class BeforeFunc, class AfterFunc>
  void exec_dfs(BeforeFunc &before, AfterFunc &after, EDGE_PROXY from_parent) {
    int node_id = from_parent.to.id;
    if (before(from_parent) || cols[node_id] != color::white)
      return;
    cols[node_id] = color::gray;
    std::vector<EDGE_PROXY> to_children;
    for (auto &next : nodes[node_id].adjacent) {
      EDGE_PROXY to_child = EDGE_PROXY(*this, node_id, next);
      exec_dfs(before, after, to_child);
      to_children.push_back(to_child);
    }
    after(from_parent, to_children);
    cols[node_id] = color::black;
  }

 public:
  graph() {}

  void resize(size_t size) {
    size_t start = nodes.size();
    nodes.resize(size);
    for (size_t i = start; i < size; i++) nodes[i].id = (int)i;
    cols.resize(size);
  }

  graph reverse() {
    graph res;
    res.resize(size());
    for (auto &n : nodes)
      for (auto &e : n.adjacent) res.add_edge(e.other_id, n.id, e.data);
    return res;
  }

  size_t size() const {
    return nodes.size();
  }
  size_t n_edges() const {
    return n_of_edges;
  }

  void clear() {
    nodes.clear();
    cols.clear();
  }

  void add_edge(int from, int to, const EDGE_DATA &data = {}) {
    auto it = std::find_if(nodes[from].adjacent.begin(), nodes[from].adjacent.end(),
                           [&](const auto &x) { return x.other_id == to; });
    if (it != nodes[from].adjacent.end()) {
      it->data = data;
    } else {
      nodes[from].add_adjacent(to, data);
      n_of_edges++;
    }
  }
  using weight_t = int64_t;
  template<typename = std::enable_if_t<std::is_convertible_v<EDGE_DATA, weight_t>>>
  EDGE_DATA get_edge(int from, int to) {
    auto it = std::find_if(nodes[from].adjacent.begin(), nodes[from].adjacent.end(),
                           [&](const auto &x) { return x.other_id == to; });
    if (it == nodes[from].adjacent.end())
      return infinity<EDGE_DATA>;
    return it->data;
  }

  template<class BeforeFunc, class AfterFunc>
  void dfs(BeforeFunc before, AfterFunc after, int start_at) {
    std::fill(cols.begin(), cols.end(), color::white);
    exec_dfs(before, after, EDGE_PROXY(*this, start_at));
  }

  template<class BeforeFunc, class AfterFunc = noop_t, class StartCallback = noop_t,
           class EndCallback = noop_t>
  void dfs(BeforeFunc before, AfterFunc after = noop, StartCallback start_callback = noop,
           EndCallback end_callback = noop) {
    std::fill(cols.begin(), cols.end(), color::white);
    for (int i = 0; i < (int)nodes.size(); i++)
      if (cols[i] == color::white) {
        start_callback();
        exec_dfs(before, after, EDGE_PROXY(*this, i));
        end_callback();
      }
  }

  std::pair<std::vector<int>, int> condense() {
    int n = (int)nodes.size();

    graph rev = reverse();

    std::vector<int> topo_sorted;
    topo_sorted.reserve(n);

    dfs([](auto) { return false; },
        [&](auto from_parent, const auto &) { topo_sorted.push_back(from_parent.to.id); });

    std::vector<int> results(n, -1);
    int comp_id = 0;
    for (int i = (int)topo_sorted.size() - 1; i >= 0; i--)
      if (int node = topo_sorted[i]; results[node] == -1) {
        rev.dfs(
            [&](auto from_parent) {
              if (results[from_parent.to.id] != -1)
                return true;
              results[from_parent.to.id] = comp_id;
              return false;
            },
            [](auto, const auto &) {}, node);
        comp_id++;
      }

    return {results, comp_id};
  }

  void exec_edmonds(int v, std::function<void(int, int, EDGE_DATA)> added_edge) {
    graph min_edges, condensed;
    std::map<std::pair<int, int>, std::pair<int, int>> condensed_edge2edge;
    std::vector<EDGE_DATA> w_prime2w(nodes.size());
    std::vector<int> v2c;
    // add all vertices in min_edges condensed vertex
    // (incomning from vertex beginning in current graph) function
    auto add_condensed_vertex = [&](int beginning) {
      min_edges.dfs(
          [&](EDGE_PROXY from_parent) -> bool {
            if (from_parent.from.is_valid() && min_edges.cols[from_parent.to.id] == color::white &&
                v2c[from_parent.to.id] == v2c[beginning])
              added_edge(from_parent.from.id, from_parent.to.id, *from_parent.edge_data);
            return v2c[from_parent.to.id] != v2c[beginning];
          },
          [](auto, const auto &) {}, beginning);
    };

    {
      graph rev = reverse();

      min_edges.resize(nodes.size());
      for (auto &node : rev.nodes) {
        EDGE_DATA min_w = infinity<EDGE_DATA>;
        for (auto &adj : node.adjacent) min_w = std::min(min_w, adj.data);
        w_prime2w[node.id] = min_w;
        for (auto &adj : node.adjacent) {
          adj.data -= min_w;
          if (adj.data == 0)
            min_edges.add_edge(adj.other_id, node.id, min_w);
        }
      }

      min_edges.dfs([](auto) { return false; }, [](auto, const auto &) {}, v);
      if (std::all_of(min_edges.cols.begin(), min_edges.cols.end(),
                      [](color col) { return col == color::black; })) {
        // Everything is reachable from v, found minimum arborescence
        min_edges.dfs(
            [&](auto from_parent) {
              if (from_parent.from.is_valid() && min_edges.cols[from_parent.to.id] == color::white)
                added_edge(from_parent.from.id, from_parent.to.id, *from_parent.edge_data);
              return false;
            }, noop, v);
        return;
      }

      // Min edges is not connected, build condensed graph
      int n_comp;
      {
        auto v2c_n_comp = min_edges.condense();
        v2c = std::move(v2c_n_comp.first);
        n_comp = v2c_n_comp.second;
      }
      condensed.resize(n_comp);
      for (auto &node : rev.nodes) {
        for (auto &adj : node.adjacent) {
          if (v2c[adj.other_id] != v2c[node.id] &&
              condensed.get_edge(v2c[adj.other_id], v2c[node.id]) > adj.data) {
            condensed.add_edge(v2c[adj.other_id], v2c[node.id], adj.data);
            condensed_edge2edge[{v2c[adj.other_id], v2c[node.id]}] = {adj.other_id, node.id};
          }
        }
      }
    }
    add_condensed_vertex(v);
    condensed.exec_edmonds(v2c[v], [&](int from, int to, EDGE_DATA weight_prime) {
      /* added edge (from, to) with current weight weight_prime */
      // add corresponding edge from current graph
      auto[real_from, real_to] = condensed_edge2edge[{from, to}];
      added_edge(real_from, real_to, weight_prime + w_prime2w[real_to]);

      // add all vertices in corresponing to (to) condensed vertex
      add_condensed_vertex(real_to);
    });
  }

  template<class AddedEdgeFunc,
    typename = std::enable_if_t<std::is_convertible_v<EDGE_DATA, weight_t>>>
  void exec_edmonds_fast(int start, AddedEdgeFunc callback) {
    int n = (int)nodes.size();

    // incoming edges
    struct local_edge {
      int from, to;
      EDGE_DATA source_w = 0, w = 0;

      bool operator<(const local_edge &rhs) const {
        return w < rhs.w ||
               (w == rhs.w && std::make_pair(from, to) < std::make_pair(rhs.from, rhs.to));
      }
      local_edge operator-(const EDGE_DATA &rhs) const {
        return {from, to, source_w, w - rhs};
      }
    };
    using skew_heap_t = skew_heap<local_edge, EDGE_DATA, std::minus<>, std::plus<EDGE_DATA>>;

    struct supervertex {
      int v;
      std::vector<int> next_level_verts;
      std::vector<local_edge> next_level_edges;
      int parent;
    };

    // combined supervertices
    disjoint_set<int> superverts(n);
    std::vector<supervertex> superv_stock(n);

    {
      std::vector<skew_heap_t> v2in_edges(n);
      for (auto &node : nodes)
        for (auto &adj : node.adjacent)
          v2in_edges[adj.other_id].insert({node.id, adj.other_id, adj.data, adj.data});

      superv_stock.reserve(2 * n);
      for (int i = 0; i < n; i++) {
        superv_stock[i].v = i;
        superverts[i] = i;
      }

      // minimum incoming edges
      std::vector<std::optional<local_edge>> v2min_in_edge(n);

      auto condense_f = [&](int from, int to) {
        std::vector<skew_heap_t *> heaps_to_merge;
        // loop through the path, being careful with possibly changing superverts.find(to)
        {
          // save path
          bool is_cycle = from == to;
          std::vector<int> path_v;
          std::vector<local_edge> path_e;
          int go_to = superverts.find(to);
          do {
            // add vertex to list
            path_v.push_back(superverts[from]);
            // add edge to list
            local_edge edge = v2min_in_edge[from].value();
            path_e.push_back(edge);
            // correct weights
            v2in_edges[from].apply_operation(edge.w);
            heaps_to_merge.push_back(&v2in_edges[from]);
            from = superverts.find(edge.from);
          } while (from != go_to);
          // add end vertex in path/not cycle
          if (!is_cycle)
            path_v.push_back(superverts[to]);

          // merge into supervertex
          for (skew_heap_t *heap : heaps_to_merge) v2in_edges[to].merge(std::move(*heap));

          for (auto &e : path_e) superverts.join(e.from, e.to);
          v2in_edges[superverts.find(to)] = std::move(v2in_edges[to]);
          for (auto v : path_v) superv_stock[v].parent = (int)superv_stock.size();
          superv_stock.push_back({-1, std::move(path_v), std::move(path_e), -1});
        }
        // merge incoming edges with fixed superverts.find(to)
        to = superverts.find(to);
        superverts[to] = (int)superv_stock.size() - 1;
        return to;
      };

      for (int i = 0; i < n; i++) {
        if (!v2min_in_edge[i].has_value()) {
          // found unused vertex, begin our path through minimum incoming edges to start

          // go through minimum incoming edges
          std::vector<bool> in_cur_path(n);
          in_cur_path[i] = true;
          int v = i;
          local_edge last_edge = {v, v};
          while (superverts.find(v) != superverts.find(start)) {
            // try to go through minimum incoming edge that is not a loop
            local_edge edge;
            {
              int last = v;
              while (v == last) {
                // find & extract minimum incoming edge, go to related supervertex
                edge = v2in_edges[v].extract_min();
                v = superverts.find(edge.from);
              }
            }
            v2min_in_edge[superverts.find(edge.to)] = last_edge = edge;

            // check cycle
            if (in_cur_path[v]) {
              // we cycled (then v != start)):
              // transform cycle into zero-weight edges cycle & condense into a supervertex
              v = condense_f(v, v);
            } else {
              in_cur_path[v] = true;
            }
            // possibly came to start, check while loop condition, otherwise continue path to start
          }

          // finally came to start, condense path into new start supervertex
          // (if it contains edges <=> it is not only start supervertex)
          if (last_edge.from != last_edge.to)
            condense_f(superverts.find(i), start);

          // go start somewhere else
        }
      }
    }

    // processed all vertices, begin cycle expanding
    std::vector<bool> used(nodes.size());
    auto expand_f = [&](supervertex &sv, int from, auto expand_f) {
      if (sv.v != -1)
        return;
      int n = (int)sv.next_level_verts.size(), m = (int)sv.next_level_edges.size();
      int start_at;
      {
        int cur = -1;
        if (from != -1) {
          cur = from;
          while (superv_stock[cur].parent != (int)(&sv - superv_stock.data()))
            cur = superv_stock[cur].parent;
        }
        // find search starting supervertex, which encloses 'from' vertex
        for (start_at = 0; start_at < n; start_at++)
          if (sv.next_level_verts[start_at] == cur)
            break;
        if (start_at == n)
          // this is start, go from list end
          start_at--;
      }
      for (int i_ = 0; i_ < n; i_++) {
        int i = (start_at - i_ + n) % n;
        // recursively search this supervertex
        expand_f(superv_stock[sv.next_level_verts[i]], from, expand_f);
        if (n == m || i - 1 >= 0) {
          // add outgoing edge
          auto &edge = sv.next_level_edges[(i - 1 + m) % m];
          if (!used[edge.to]) {
            callback(edge.from, edge.to, edge.source_w);
            used[edge.to] = true;
          }
          from = edge.to;
        }
      }
    };
    used[start] = true;
    expand_f(superv_stock[superverts[start]], -1, expand_f);
  }

  template<typename = std::enable_if_t<std::is_convertible_v<EDGE_DATA, weight_t>>>
  weight_t edmonds(int v) {
    dfs([](auto) { return false; }, [](auto, const auto &) {}, v);
    for (int i = 0; i < (int)nodes.size(); i++)
      if (cols[i] != color::black)
        return infinity<weight_t>;

    weight_t res = 0;
    size_t counter = 0;
    std::cout << "Ol' reliable:" << std::endl;
    exec_edmonds(v, [&]([[maybe_unused]] int from, [[maybe_unused]] int to, EDGE_DATA w) {
      std::cout << from + 1 << ' ' << to + 1 << std::endl;
      res += w;
      counter++;
    });
    assert(counter == nodes.size() - 1);
    return res;
  }

  template<typename = std::enable_if_t<std::is_convertible_v<EDGE_DATA, weight_t>>>
  weight_t edmonds_fast(int v) {
    dfs([](auto) { return false; }, [](auto, const auto &) {}, v);
    for (int i = 0; i < (int)nodes.size(); i++)
      if (cols[i] != color::black)
        return infinity<weight_t>;

    weight_t res = 0;
    size_t counter = 0;
    // std::cout << "Fast:" << std::endl;
    exec_edmonds_fast(v, [&]([[maybe_unused]] int from, [[maybe_unused]] int to, EDGE_DATA w) {
      // std::cout << from + 1 << ' ' << to + 1 << std::endl;
      res += w;
      counter++;
    });
    assert(counter == nodes.size() - 1);
    return res;
  }

  const NODE_DATA &operator[](size_t ind) const {
    return nodes[ind].data;
  }
  NODE_DATA &operator[](size_t ind) {
    return nodes[ind].data;
  }
};

struct NODE_DATA_T {};
using EDGE_DATA_T = int;

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

graph_t read_graph(std::istream &in) {
  int n, m;
  in >> n >> m;

  graph_t g;
  g.resize(n);

  for (int i = 0; i < m; i++) {
    int from, to, w;
    in >> from >> to >> w;
    --from, --to;
    if (from != to && g.get_edge(from, to) > w)
      g.add_edge(from, to, w);
  }
  return std::move(g);
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

#if 0
  for (int i = 0; i < 1000000; i++) {
    DeleteFile(L"in.txt");
    system("C:\\Ruby27-x64\\bin\\ruby.exe a.rb > in.txt");
    std::ifstream in("in.txt");

    graph_t g = read_graph(in);
    int64_t min_w = g.edmonds(0), mw = g.edmonds_fast(0);
    if (min_w != mw) {
      std::cout << "wot" << std::endl;
      return 0;
    }
    std::cout << "k" << std::endl;
    /*
    if (min_w == infinity<int64_t>)
      std::cout << "NO" << std::endl;
    else
      std::cout << "YES" << std::endl << min_w << std::endl;
    */
  }
#else
    graph_t g = read_graph(std::cin);
    int64_t min_w = /*g.edmonds(0), mw = */g.edmonds_fast(0);
    if (min_w == infinity<int64_t>)
      std::cout << "NO" << std::endl;
    else
      std::cout << "YES" << std::endl << min_w << std::endl;
#endif
  return 0;
}
