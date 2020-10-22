/* Kholiavin Nikolai, M3138 */
#include <functional>
#include <iostream>
#include <numeric>
#include <vector>
#include <list>
#include <set>
#include <stack>
#include <algorithm>
#include <ios>

template<class DATA>
class graph {
 private:
  struct NODE {
    int id;
    DATA data;
    std::vector<std::pair<int, int>> adjacent;

    NODE(int id = -1) : id(id) {}

    void add_adjacent(int other_id, int weight) {
      adjacent.push_back({other_id, weight});
    }
  };

 private:
  std::vector<NODE> nodes;

  void exec_dfs(const std::function<bool(int id, DATA *parent_data, DATA &node_data)> &before,
                const std::function<void(int id, const std::vector<std::pair<DATA *, int>>
                  &ch_data, DATA &node_data)> &after,
                DATA *parent_data,
                int id) {
    if (before(id, parent_data, nodes[id].data) || cols[id]) return;
    std::vector<std::pair<DATA *, int>> ch_data;
    for (auto next : nodes[id].adjacent) {
      exec_dfs(before, after, &nodes[id].data, next.first);
      ch_data.push_back({&nodes[next.first].data, next.second});
    }
    after(id, ch_data, nodes[id].data);
    cols[id] = true;
  }

 public:
  std::vector<bool> cols;

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
      for (auto e : n.adjacent) res.add_edge(e.first, n.id, e.second);
    return res;
  }

  size_t size() const { return nodes.size(); }

  void clear() { nodes.clear(); }

  void add_edge(int from, int to, int weight) {
    nodes[from].add_adjacent(to, weight);
  }

  static graph read_graph(std::istream &i, int &S, int &T) {
    graph g;

    int N, M;
    i >> N >> M >> S >> T;
    S--, T--;

    g.resize(N);
    for (int j = 0; j < M; j++) {
      int adj1, adj2, w;
      i >> adj1 >> adj2 >> w;
      g.add_edge(adj1 - 1, adj2 - 1, w);
    }
    return g;
  }

  void dfs(const std::function<bool(int id, DATA *parent_data, DATA &node_data)> &before,
           const std::function<void(int id, const std::vector<std::pair<DATA *, int>> &ch_data,
             DATA &node_data)> &after,
           int start_at) {
    std::fill(cols.begin(), cols.end(), 0);
    exec_dfs(before, after, nullptr, start_at);
  }

  const DATA & operator[](size_t ind) const { return nodes[ind].data; }
  DATA & operator[](size_t ind) { return nodes[ind].data; }
};

constexpr int inf = std::numeric_limits<int>::max();

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  struct DATA {
    int min_length = inf;
  };
  using graph_t = graph<DATA>;

  int s, t;
  graph_t g = graph_t::read_graph(std::cin, s, t);

  g.dfs(
    [&](int, DATA *, DATA &) {
      return false;
    },
    [&](int id, const std::vector<std::pair<DATA *, int>> &ch_data, DATA &data) {
      if (id == t) {
        data.min_length = 0;
      } else {
        data.min_length = inf;
        for (auto child : ch_data)
          if (child.first->min_length != inf)
            data.min_length = std::min(data.min_length, child.first->min_length + child.second);
      }
    }, s);

  if (g[s].min_length == inf)
    std::cout << "Unreachable";
  else
    std::cout << g[s].min_length;
  std::cout << std::endl;

  return 0;
}
