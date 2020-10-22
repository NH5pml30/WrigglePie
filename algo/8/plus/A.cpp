/* Kholiavin Nikolai, M3138 */
#include <functional>
#include <iostream>
#include <numeric>
#include <vector>

class graph {
 private:
  struct NODE {
    int id;
    std::vector<int> adjacent;

    NODE(int id, std::vector<int> adjacent)
        : id(id), adjacent(std::move(adjacent)) {}

    NODE(int id = -1) : id(id) {}

    void add_adjacent(int other_id) { adjacent.push_back(other_id); }
  };

 public:
  enum class color { white, gray, black };

 private:
  std::vector<NODE> nodes;
  std::vector<color> colors;

  void exec_dfs(const std::function<void(int id, color col)> &before,
                const std::function<void(int id)> &after, int id) {
    before(id, colors[id]);
    if (colors[id] != color::white) return;
    colors[id] = color::gray;
    for (auto next : nodes[id].adjacent) exec_dfs(before, after, next);
    colors[id] = color::black;
    after(id);
  }

 public:
  graph() {}

  void resize(size_t size) {
    size_t start = nodes.size();
    nodes.resize(size);
    for (size_t i = start; i < size; i++) nodes[i].id = (int)i;
    colors.resize(size);
  }

  void clear() {
    nodes.clear();
    colors.clear();
  }

  void add_edge(int from, int to) { nodes[from].add_adjacent(to); }

  friend std::istream &operator>>(std::istream &i, graph &g);

  void dfs(const std::function<void(int id, color col)> &before,
           const std::function<void(int id)> &after) {
    std::fill(colors.begin(), colors.end(), color::white);

    for (auto &n : nodes)
      if (colors[n.id] == color::white) exec_dfs(before, after, n.id);
  }
};

std::istream &operator>>(std::istream &i, graph &g) {
  g.clear();

  int N, M;
  i >> N >> M;

  g.resize(N);
  for (int j = 0; j < M; j++) {
    int adj1, adj2;
    i >> adj1 >> adj2;
    g.add_edge(adj1 - 1, adj2 - 1);
  }
  return i;
}

int main() {
  graph g;
  std::cin >> g;

  std::vector<int> topo_sorted;
  bool is_ok = true;

  g.dfs(
      [&](int, graph::color col) {
        if (col == graph::color::gray) is_ok = false;
      },
      [&](int id) {
        if (is_ok) topo_sorted.push_back(id);
      });
  if (is_ok) {
    for (auto it = topo_sorted.rbegin(); it != topo_sorted.rend(); it++)
      std::cout << *it + 1 << ' ';
  } else {
    std::cout << -1;
  }
  std::cout << std::endl;
  return 0;
}
