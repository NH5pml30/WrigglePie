/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <iostream>
#include <limits>
#include <queue>
#include <list>
#include <tuple>
#include <vector>
#include <set>
#include <map>
#include <cmath>
#include <iomanip>
#include <optional>

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

  bool join(int x, int y) {
    x = find(x), y = find(y);
    if (x == y)
      return false;

    NODE_DATA::attach(nodes[x], nodes[y]);
    return true;
  }
};

struct empty {
};

template<class NODE_DATA = empty, class EDGE_DATA = empty>
class graph {
 private:
  struct NODE {
    int id;
    NODE_DATA data;
    std::vector<std::pair<int, EDGE_DATA>> adjacent;

    NODE(int id = -1) : id(id) {}

    void add_adjacent(int other_id, const EDGE_DATA &data) {
      adjacent.push_back({other_id, data});
    }
    typename std::vector<std::pair<int, EDGE_DATA>>::iterator find_adjacent(
      int other_id) {
      return std::find_if(adjacent.begin(), adjacent.end(),
                          [&](const std::pair<int, EDGE_DATA> &cur) {
                            return cur.first == other_id;
                          });
    }
  };

 public:
  enum class color {
    white, gray, black
  };
  std::vector<color> cols;

 private:
  std::vector<NODE> nodes;

 public:
  class NODE_PROXY {
    friend class EDGE_PROXY;

   public:
    const int id;
    NODE_DATA &data;
    const color &col;

    // private:
    static NODE_DATA null_data;

    NODE_PROXY(graph &g, int id)
        : id(id),
          data(id >= 0 ? g.nodes[id].data : null_data),
          col(id >= 0 ? g.cols[id] : static_cast<const color &>(color())) {}
  };

  class EDGE_PROXY {
    friend class graph;

   public:
    const NODE_PROXY from, to;
    EDGE_DATA &edge_data;

    // private:
    static EDGE_DATA null_data;

    EDGE_PROXY(graph &g, int from, int to, EDGE_DATA &edge_data)
        : from(g, from), to(g, to), edge_data(edge_data) {}

    EDGE_PROXY(graph &g, int to) : from(g, -1), to(g, to), edge_data(null_data) {}
  };

  template<class BeforeFunc, class AfterFunc>
  void exec_dfs(BeforeFunc &before,
                AfterFunc &after,
                EDGE_PROXY from_parent) {
    int node_id = from_parent.to.id;
    if (before(from_parent) || cols[node_id] != color::white) return;
    cols[node_id] = color::gray;
    std::vector<EDGE_PROXY> to_children;
    for (auto next : nodes[node_id].adjacent) {
      EDGE_PROXY to_child = EDGE_PROXY(*this, node_id, next.first, next.second);
      exec_dfs(before, after, to_child);
      to_children.push_back(to_child);
    }
    after(from_parent.to, to_children);
    cols[node_id] = color::black;
  }

  graph() {}

  void resize(size_t size) {
    size_t start = nodes.size();
    nodes.resize(size);
    cols.resize(size);
    for (size_t i = start; i < size; i++) nodes[i].id = (int)i;
  }

  graph reverse() {
    graph res;
    res.resize(size());
    for (auto &n : nodes)
      for (auto &e : n.adjacent) res.add_edge(e.first, n.id, e.second);
    return res;
  }

  size_t size() const { return nodes.size(); }

  void clear() {
    nodes.clear();
    cols.clear();
  }

  void add_edge(int from, int to, const EDGE_DATA &data = {}) {
    auto it = nodes[from].find_adjacent(to);
    if (it == nodes[from].adjacent.end()) nodes[from].add_adjacent(to, data);
    else it->second = data;
  }
  EDGE_DATA * get_edge(int from, int to) {
    auto it = nodes[from].find_adjacent(to);
    if (it == nodes[from].adjacent.end()) return nullptr;
    return &it->second;
  }

  template<class BeforeFunc, class AfterFunc>
  void dfs(BeforeFunc before, AfterFunc after, int start_at) {
    std::fill(cols.begin(), cols.end(), color::white);
    exec_dfs(before, after, EDGE_PROXY(*this, start_at));
  }

  template<class BeforeFunc, class AfterFunc, class Callback>
  void dfs(BeforeFunc before, AfterFunc after, Callback again_callback) {
    std::fill(cols.begin(), cols.end(), color::white);
    for (int i = 0; i < (int)nodes.size(); i++)
      if (cols[i] == color::white) {
        again_callback();
        exec_dfs(before, after, EDGE_PROXY(*this, i));
      }
  }

  std::pair<std::vector<int>, int> find_components() {
    std::vector<int> v2c(size());
    int cur_comp = -1;

    dfs([&](EDGE_PROXY) { return false; },
        [&](NODE_PROXY self,
            const std::vector<EDGE_PROXY> &) {
          v2c[self.id] = cur_comp;
        },
        [&]() { cur_comp++; });

    return {std::move(v2c), cur_comp + 1};
  }

  const NODE_DATA & operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA & operator[](size_t ind) { return nodes[ind].data; }
};

template<class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template<class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

using graph_t = graph<empty, int>;

std::pair<std::vector<std::pair<int, std::pair<int, int>>>, int> read_edges(std::istream &in) {
  int n, m;
  in >> n >> m;

  std::vector<std::pair<int, std::pair<int, int>>> res;
  for (int i = 0; i < m; i++) {
    int from, to, w;
    in >> from >> to >> w;
    --from, --to;
    if (from == to) continue;
    if (from > to) std::swap(from, to);
    res.push_back({w, {from, to}});
  }
  std::sort(res.begin(), res.end());
  return {std::move(res), n};
}

template<typename T>
constexpr T infinity = std::numeric_limits<T>::max();

template<class InputIt, class ConverterFunc>
graph_t build_graph(int n, InputIt begin, InputIt end, ConverterFunc converter) {
  graph_t g;
  g.resize(n);

  for (auto it = begin; it != end; it++) {
    int u = converter(it->second.first), v = converter(it->second.second);
    if (auto w = g.get_edge(u, v); w == nullptr || *w > it->first) {
      g.add_edge(u, v, it->first);
      g.add_edge(v, u, it->first);
    }
  }
  return g;
}

template<class InputIt>
std::optional<int> mbst(int n, InputIt edges_begin, InputIt edges_end) {
  disjoint_set ds(n);

  auto it = edges_begin;
  for (; it != edges_end; it++) {
    n -= ds.join(it->second.first, it->second.second);
    if (n == 1)
      break;
  }
  if (n > 1)
    return {};
  return it->first;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto[edges, n] = read_edges(std::cin);

  int diff = infinity<int>;
  for (int e_begin = 0; e_begin < (int)edges.size(); e_begin++) {
    auto max_edge = mbst(n, edges.begin() + e_begin, edges.end());
    if (!max_edge.has_value()) break;

    diff = std::min(diff, max_edge.value() - edges[e_begin].first);
  }

  if (diff == infinity<int>) {
    std::cout << "NO" << std::endl;
  } else {
    std::cout << "YES" << std::endl << diff << std::endl;
  }
  return 0;
}
