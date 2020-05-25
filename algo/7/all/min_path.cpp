/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <stack>
#include <set>
#include <algorithm>

struct node {
  static std::vector<node> storage;
  struct f_res {
    int v;
    int min_weight;

    f_res() {}

    f_res(int v, int min_weight) : v(v), min_weight(min_weight) {}

    f_res compose(const f_res &other) const {
      return {other.v, std::min(min_weight, other.min_weight)};
    }

    f_res &compose_assign(const f_res &other) {
      return *this = compose(other);
    }

    f_res jump(int pow) const;
    f_res & jump_assign(int pow);

    bool operator==(const f_res &other) const { return v == other.v; }

    bool operator!=(const f_res &other) const { return !(*this == other); }
  };
  int parent, depth;
  int edge_weight;
  std::vector<f_res> fg;
};

node::f_res node::f_res::jump(int pow) const {
  return compose(node::storage[v].fg[pow]);
}

node::f_res & node::f_res::jump_assign(int pow) {
  return compose_assign(node::storage[v].fg[pow]);
}

std::vector<node> node::storage;

unsigned clog2(unsigned x) {
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x++;

  unsigned res = 0;
  for (; x > 0; x >>= 1, res++) {
  }
  return res - 1;
}

int main() {
  constexpr int infinity = std::numeric_limits<int>::max();

  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n;
  std::cin >> n;
  int logn = clog2(n - 1);
  auto &nodes = node::storage;
  nodes.resize(n);

  {
    std::vector<std::vector<int>> children(n);

    nodes[0].parent = 0;
    for (int i = 0; i < n - 1; i++) {
      int pi, w;
      std::cin >> pi >> w;
      pi--;
      children[pi].push_back(i + 1);
      nodes[i + 1].parent = pi;
      nodes[i + 1].edge_weight = w;
    }

    std::stack<std::pair<int, int>> stack;
    stack.push({0, 0});
    while (!stack.empty()) {
      auto[v, d] = stack.top();
      stack.pop();
      nodes[v].fg = {{nodes[v].parent, nodes[v].edge_weight}};
      nodes[v].fg.resize(logn + 1);
      nodes[v].depth = d;
      for (auto c : children[v])
        stack.push({c, d + 1});
    }
  }

  for (int i = 1; i <= logn; i++)
    for (int j = 0; j < n; j++)
      nodes[j].fg[i] = nodes[j].fg[i - 1].jump(i - 1);

  int m;
  std::cin >> m;
  for (int i = 0; i < m; i++) {
    int u, v;
    std::cin >> u >> v;
    node::f_res first = {u - 1, infinity}, second = {v - 1, infinity};
    int diff = nodes[first.v].depth - nodes[second.v].depth;
    if (diff != 0) {
      if (diff < 0) {
        std::swap(first, second);
        diff = -diff;
      }
      int pow = 0;
      for (; diff > 0; diff >>= 1, pow++)
        if (diff & 1) first.jump_assign(pow);
    }

    if (first == second) {
      std::cout << first.min_weight << '\n';
    } else {
      for (int jump = logn; jump >= 0; jump--)
        if (first.jump(jump) != second.jump(jump)) {
          first.jump_assign(jump);
          second.jump_assign(jump);
        }
      first.jump_assign(0);
      second.jump_assign(0);
      std::cout << std::min(first.min_weight, second.min_weight) << '\n';
    }
  }

  std::cout.flush();
  return 0;
}
