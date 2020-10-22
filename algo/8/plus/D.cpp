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
    std::vector<int> adjacent;

    NODE(int id = -1) : id(id) {}

    void add_adjacent(int other_id) {
      adjacent.push_back(other_id);
    }
  };

 private:
  std::vector<NODE> nodes;

  void exec_dfs(const std::function<bool(int id, DATA *parent_data, DATA &node_data)> &before,
                const std::function<void(int id, const std::vector<DATA *> &ch_data,
                  DATA &node_data)> &after,
                DATA *parent_data,
                int id) {
    if (before(id, parent_data, nodes[id].data) || cols[id]) return;
    std::vector<DATA *> ch_data;
    for (auto next : nodes[id].adjacent) {
      exec_dfs(before, after, &nodes[id].data, next);
      ch_data.push_back(&nodes[next].data);
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

  void add_edge(int from, int to) {
    nodes[from].add_adjacent(to);
  }

  static graph read_graph(std::istream &i, int &S) {
    graph g;

    int N, M;
    i >> N >> M >> S;
    S--;

    g.resize(N);
    for (int j = 0; j < M; j++) {
      int adj1, adj2;
      i >> adj1 >> adj2;
      g.add_edge(adj1 - 1, adj2 - 1);
    }
    return g;
  }

  void dfs(const std::function<bool(int id, DATA *parent_data, DATA &node_data)> &before,
           const std::function<void(int id, const std::vector<DATA *> &ch_data, DATA &node_data)>
             &after,
           int start_at) {
    std::fill(cols.begin(), cols.end(), 0);
    exec_dfs(before, after, nullptr, start_at);
  }

  const DATA & operator[](size_t ind) const { return nodes[ind].data; }
  DATA & operator[](size_t ind) { return nodes[ind].data; }
};

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  struct DATA {
    bool first_wins;
  };
  using graph_t = graph<DATA>;

  int s;
  graph_t g = graph_t::read_graph(std::cin, s);

  g.dfs(
    [&](int, DATA *, DATA &) {
      return false;
    },
    [&](int, const std::vector<DATA *> &ch_data, DATA &data) {
      if (ch_data.empty()) {
        // leaf
        data.first_wins = false;
      } else {
        // non-leaf node
        bool wins = false;
        for (auto child : ch_data)
          if (!child->first_wins) {
            // choose to win
            wins = true;
            break;
          }
        data.first_wins = wins;
      }
    }, s);

  std::cout << (g[s].first_wins ? "First" : "Second") << " player wins" << std::endl;

  return 0;
}
