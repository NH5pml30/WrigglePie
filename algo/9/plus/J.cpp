/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <numeric>
#include <iostream>
#include <limits>
#include <queue>
#include <tuple>
#include <vector>
#include <map>
#include <functional>

template <typename T = int>
constexpr T infinity = std::numeric_limits<T>::max();

template <typename T = int>
constexpr T neg_infinity = std::numeric_limits<T>::min();

struct empty {};

auto noop = [] {};
using noop_t = decltype(noop);

template <class NODE_DATA = empty, class EDGE_DATA = empty>
class graph {
 private:
  struct NODE {
    struct EDGE_ENTRY {
      int other_id;
      EDGE_DATA data;

      EDGE_ENTRY() {}
      EDGE_ENTRY(int other_id, const EDGE_DATA &data)
          : other_id(other_id), data(data) {}
      EDGE_ENTRY(int other_id, EDGE_DATA &&data)
          : other_id(other_id), data(std::move(data)) {}

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
  enum class color {
    white, gray, black
  };
  std::vector<color> cols;

  class NODE_PROXY {
    friend class EDGE_PROXY;

   public:
    const int id;
    NODE_DATA &data;

    // private:
    static NODE_DATA null_data;

    bool is_valid() const { return &data != &null_data; }

    NODE_PROXY(graph &g, int id)
        : id(id), data(id >= 0 ? g.nodes[id].data : null_data) {}
  };

  class EDGE_PROXY {
    friend class graph;

   public:
    const NODE_PROXY from, to;
    EDGE_DATA &edge_data;

    // private:
    static EDGE_DATA null_data;

    EDGE_PROXY(graph &g, int from, typename NODE::EDGE_ENTRY &entry)
        : from(g, from), to(g, entry.other_id), edge_data(entry.data) {}

    EDGE_PROXY(graph &g, int to)
        : from(g, -1), to(g, to), edge_data(null_data) {}
  };

 private:
  template<class BeforeFunc, class AfterFunc>
  void exec_dfs(BeforeFunc &before,
                AfterFunc &after,
                EDGE_PROXY from_parent) {
    int node_id = from_parent.to.id;
    if (before(from_parent) || cols[node_id] != color::white) return;
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

  size_t size() const { return nodes.size(); }
  size_t n_edges() const { return n_of_edges; }

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
    if (it == nodes[from].adjacent.end()) return infinity<EDGE_DATA>;
    return it->data;
  }

  template<class BeforeFunc, class AfterFunc>
  void dfs(BeforeFunc before, AfterFunc after, int start_at) {
    std::fill(cols.begin(), cols.end(), color::white);
    exec_dfs(before, after, EDGE_PROXY(*this, start_at));
  }

  template<class BeforeFunc, class AfterFunc, class StartCallback = noop_t,
           class EndCallback = noop_t>
  void dfs(
      BeforeFunc before, AfterFunc after, StartCallback start_callback = noop,
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
        [&](auto from_parent, const auto &) {
          topo_sorted.push_back(from_parent.to.id);
        });

    std::vector<int> results(n, -1);
    int comp_id = 0;
    for (int i = (int)topo_sorted.size() - 1; i >= 0; i--)
      if (int node = topo_sorted[i]; results[node] == -1) {
        rev.dfs(
            [&](auto from_parent) {
              if (results[from_parent.to.id] != -1) return true;
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
            if (from_parent.from.is_valid() &&
                min_edges.cols[from_parent.to.id] == color::white &&
                v2c[from_parent.to.id] == v2c[beginning])
              added_edge(from_parent.from.id, from_parent.to.id,
                          from_parent.edge_data);
            return v2c[from_parent.to.id] != v2c[beginning];
          },
          [](auto, const auto &) {}, beginning);
    };

    {
      graph rev = reverse();

      min_edges.resize(nodes.size());
      for (auto &node : rev.nodes) {
        EDGE_DATA min_w = infinity<EDGE_DATA>;
        for (auto &adj : node.adjacent)
          min_w = std::min(min_w, adj.data);
        w_prime2w[node.id] = min_w;
        for (auto &adj : node.adjacent) {
          adj.data -= min_w;
          if (adj.data == 0) min_edges.add_edge(adj.other_id, node.id, min_w);
        }
      }

      min_edges.dfs([](auto) { return false; }, [](auto, const auto &) {}, v);
      if (std::all_of(min_edges.cols.begin(), min_edges.cols.end(),
                      [](color col) { return col == color::black; })) {
        // Everything is reachable from v, found minimum arborescence
        min_edges.dfs(
            [&](auto from_parent) {
              if (from_parent.from.is_valid() &&
                  min_edges.cols[from_parent.to.id] == color::white)
                added_edge(from_parent.from.id, from_parent.to.id,
                           from_parent.edge_data);
              return false;
            },
            [](auto, const auto &) {}, v);
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
            condensed_edge2edge[{v2c[adj.other_id], v2c[node.id]}] = {
                adj.other_id, node.id};
          }
        }
      }
    }
    add_condensed_vertex(v);
    condensed.exec_edmonds(
        v2c[v], [&](int from, int to, EDGE_DATA weight_prime) {
          /* added edge (from, to) with current weight weight_prime */
          // add corresponding edge from current graph
          auto[real_from, real_to] = condensed_edge2edge[{from, to}];
          added_edge(real_from, real_to, weight_prime + w_prime2w[real_to]);

          // add all vertices in corresponing to (to) condensed vertex
          add_condensed_vertex(real_to);
        });
  }

  template<typename = std::enable_if_t<std::is_convertible_v<EDGE_DATA, weight_t>>>
  weight_t edmonds(int v) {
    dfs([](auto) { return false; }, [](auto, const auto &) {}, v);
    for (int i = 0; i < (int)nodes.size(); i++)
      if (cols[i] != color::black) return infinity<weight_t>;

    weight_t res = 0;
    exec_edmonds(v, [&](int, int, EDGE_DATA w) {
        res += w;
      });
    return res;
  }


  const NODE_DATA &operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA &operator[](size_t ind) { return nodes[ind].data; }
};

template <class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template <class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

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

  graph_t g = read_graph(std::cin);
  int64_t min_w = g.edmonds(0);
  if (min_w == infinity<int64_t>) std::cout << "NO" << std::endl;
  else
    std::cout << "YES" << std::endl << min_w << std::endl;
  return 0;
}
