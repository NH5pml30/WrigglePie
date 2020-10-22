/* Kholiavin Nikolai, M3138 */
#include <functional>
#include <iostream>
#include <numeric>
#include <vector>
#include <string>
#include <set>

class graph {
 private:
  struct NODE {
    int id;
    std::vector<int> adjacent;

    NODE(int id, std::vector<int> adjacent)
        : id(id), adjacent(std::move(adjacent)) {}

    NODE(int id = -1) : id(id) {}

    void add_adjacent(int other_id) {
      if (std::find(adjacent.begin(), adjacent.end(), other_id) == adjacent.end())
        adjacent.push_back(other_id);
    }
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
      if (colors[n.id] == color::white)
        exec_dfs(before, after, n.id);
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
  g.resize(26);

  int n;
  std::cin >> n;

  std::set<int> can_be_zero;
  for (int i = 0; i < 26; i++)
    can_be_zero.insert(i);

  int last_len = 0;
  std::string last_number = "";
  for (int i = 0; i < n; i++) {
    std::string number;
    std::cin >> number;
    if (number.length() > 1)
      can_be_zero.erase(number.front() - 'a');
    if ((int)number.length() < last_len) {
      std::cout << "No" << std::endl;
      return 0;
    } else if ((int)number.length() == last_len) {
      int i = 0;
      for (; i < last_len && last_number[i] == number[i]; i++) {
      }
      if (i == last_len) {
        std::cout << "No" << std::endl;
        return 0;
      }
      g.add_edge(last_number[i] - 'a', number[i] - 'a');
      can_be_zero.erase(number[i] - 'a');
    }
    last_number = number;
    last_len = (int)number.length();
  }

  if (can_be_zero.empty()) {
    std::cout << "No" << std::endl;
    return 0;
  }

  int zero = *can_be_zero.begin();

  for (int i = 0; i < 26; i++)
    if (i != zero)
      g.add_edge(zero, i);

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
    std::cout << "Yes" << std::endl;
    std::vector<int> id2pos(26);
    int pos = 0;
    for (auto it = topo_sorted.rbegin(); it != topo_sorted.rend(); it++)
      id2pos[*it] = pos++;
    for (int i = 0; i < 26; i++)
      std::cout << id2pos[i] << ' ';
  } else {
    std::cout << "No";
  }
  std::cout << std::endl;
  return 0;
}
