/* Kholiavin Nikolai, M3138 */
#include <functional>
#include <iostream>
#include <numeric>
#include <vector>
#include <list>
#include <set>
#include <stack>
#include <algorithm>

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
  mutable std::vector<color> colors;

  void exec_dfs(const std::function<bool(int id, color col)> &before,
                const std::function<void(int id)> &after, int id) const {
    bool bail = before(id, colors[id]);
    if (colors[id] != color::white || bail) return;
    colors[id] = color::gray;
    for (auto next : nodes[id].adjacent) exec_dfs(before, after, next);
    colors[id] = color::black;
    after(id);
  }

 public:
  graph() {}

  graph reverse() const {
    graph res;
    res.resize(nodes.size());
    for (auto &n : nodes)
      for (auto next : n.adjacent)
        res.add_edge(next, n.id);
    return res;
  }

  void resize(size_t size) {
    size_t start = nodes.size();
    nodes.resize(size);
    for (size_t i = start; i < size; i++) nodes[i].id = (int)i;
    colors.resize(size);
  }

  size_t size() const { return nodes.size(); }

  void clear() {
    nodes.clear();
    colors.clear();
  }

  void add_edge(int from, int to) { nodes[from].add_adjacent(to); }

  friend std::istream &operator>>(std::istream &i, graph &g);

  void dfs(const std::function<bool(int id, color col)> &before,
           const std::function<void(int id)> &after, int start_at = -1) const {
    std::fill(colors.begin(), colors.end(), color::white);

    if (start_at == -1) {
      for (auto &n : nodes)
        if (colors[n.id] == color::white)
          exec_dfs(before, after, n.id);
    } else {
      exec_dfs(before, after, start_at);
    }
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

  graph rev = g.reverse();

  std::vector<int> *save_slot = nullptr;

  std::vector<int> topo_sorted;

  save_slot = &topo_sorted;

  static auto aft3r =
    [&](int id) {
      save_slot->push_back(id);
    };
  static auto b4 = [](auto, auto) { return false; };

  g.dfs(b4, aft3r);

  std::vector<int> results(g.size(), -1);

  std::vector<int> component;
  save_slot = &component;
  int comp_id = 0;
  while (!topo_sorted.empty()) {
    int node = topo_sorted.back();
    component.clear();
    rev.dfs([&](int id, graph::color) {
        return results[id] != -1;
      }, aft3r, node);

    std::set<int> in_comp(component.begin(), component.end());
    topo_sorted.erase(std::remove_if(topo_sorted.begin(), topo_sorted.end(),
      [&](int id) -> bool { return in_comp.count(id); }), topo_sorted.end());
    for (auto el : component)
      results[el] = comp_id;
    comp_id++;
  }

  std::cout << comp_id << std::endl;
  for (auto el : results)
    std::cout << el + 1 << ' ';
  std::cout << std::endl;
  return 0;
}
