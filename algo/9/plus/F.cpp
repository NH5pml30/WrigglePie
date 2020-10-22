/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <iostream>
#include <limits>
#include <queue>
#include <tuple>
#include <vector>
#include <set>

template <typename T = int>
constexpr T infinity = std::numeric_limits<T>::max();

template <typename T = int>
constexpr T neg_infinity = std::numeric_limits<T>::min();

struct empty {};

template<typename weight_t>
struct number {
  static constexpr weight_t inf = infinity<weight_t>, neg_inf = neg_infinity<weight_t>;
  weight_t data;

  number(weight_t d) : data(d) {}

  number operator+(number other) const {
    if (other.data > 0 && data > inf - other.data) return inf;
    if (other.data < 0 && data < neg_inf - other.data) return neg_inf;
    return data + other.data;
  }

  bool operator<(number other) const {
    return data < other.data;
  }
  bool operator>(number other) const {
    return data > other.data;
  }
  bool is_inf() const {
    return data == inf;
  }
  bool is_neg_inf() const {
    return data == neg_inf;
  }
  bool is_nan() const {
    return is_inf() || is_neg_inf();
  }
  bool operator==(number other) const {
    return is_nan() || other.is_nan() ? false : data == other.data;
  }
  bool operator!=(number other) const {
    return is_nan() || other.is_nan() ? false : data != other.data;
  }
};

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

  graph() {}

  void resize(size_t size) {
    size_t start = nodes.size();
    nodes.resize(size);
    for (size_t i = start; i < size; i++) nodes[i].id = (int)i;
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

  void clear() { nodes.clear(); }

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

  using weight_t = int;
  std::enable_if_t<std::is_same_v<EDGE_DATA, weight_t>,
                   std::vector<int>>
  find_negative_cycle() {
    // Floyd-Warshall
    int n = (int)nodes.size();
    using number_t = number<weight_t>;
    std::vector<std::vector<std::pair<number_t, int>>> mat(n);
    for (int i = 0; i < n; i++) mat[i].resize(n, {number_t::inf, -1});
    for (auto &node : nodes) {
      mat[node.id][node.id] = {0, node.id};
      for (auto &next : node.adjacent)
        mat[node.id][next.other_id] = {next.data, next.other_id};
    }

    for (int k = 0; k < n; k++)
      for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
          if (!mat[i][k].first.is_inf() && !mat[k][j].first.is_inf() &&
              mat[i][j].first > mat[i][k].first + mat[k][j].first)
            mat[i][j] = {mat[i][k].first + mat[k][j].first, mat[i][k].second};

    std::vector<int> cycle;
    std::vector<bool> was(nodes.size());
    for (int x = 0; x < n; x++)
      if (mat[x][x].first < 0) {
        // x is on a negative cycle, go through it
        int cur = x;
        while (!was[cur]) {
          was[cur] = true;
          cycle.push_back(cur);
          cur = mat[cur][x].second;
        }
        int i;
        for (i = 0; i < (int)cycle.size(); i++)
          if (cur == cycle[i])
            break;
        cycle.erase(cycle.begin(), cycle.begin() + i);
        return cycle;
      }
    return {};
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
  int n;
  in >> n;
  graph_t g;
  g.resize(n);

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++) {
      int w;
      in >> w;
      if (w <= 10000) g.add_edge(i, j, w);
    }
  return std::move(g);
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  graph_t g = read_graph(std::cin);
  std::vector<int> neg_cycle = g.find_negative_cycle();
  if (!neg_cycle.empty()) {
    std::cout << "YES" << std::endl << neg_cycle.size() + 1 << std::endl;
    for (auto el : neg_cycle) std::cout << el + 1 << ' ';
    std::cout << neg_cycle.front() + 1 << std::endl;
  } else {
    std::cout << "NO" << std::endl;
  }
  return 0;
}
