/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>

class disjoint_set {
 private:
  struct NODE_DATA {
    int index, parent, rank;
    int val = 0, delta = 0;

    NODE_DATA(int index) :
      index(index), parent(index), rank(index) {
    }

    NODE_DATA() {
    }

    static void attach(NODE_DATA &X, NODE_DATA &Y) {
      if (X.rank > Y.rank) {
        Y.parent = X.index;
        Y.delta -= X.delta;
      } else {
        X.parent = Y.index;
        if (X.rank == Y.rank)
          Y.rank++;
        X.delta -= Y.delta;
      }
    }
  };

  std::vector<NODE_DATA> nodes;

  std::pair<int, int> findUpdate(int x) {
    if (nodes[x].parent == x)
      return {x, 0};
    auto save = findUpdate(nodes[x].parent);
    return {nodes[x].parent = save.first, nodes[x].delta += save.second};
  }

 public:
  disjoint_set(int n) : nodes(n) {
    int i = 0;
    for (auto &el : nodes)
      el = NODE_DATA(i++);
  }

  int find(int x) {
    return findUpdate(x).first;
  }

  std::pair<int, int> get(int x) {
    auto save = findUpdate(x);
    return {save.first, save.second + nodes[save.first].delta + nodes[x].val};
  }

  bool join(int x, int y) {
    x = find(x), y = find(y);
    if (x == y)
      return false;

    NODE_DATA::attach(nodes[x], nodes[y]);
    return true;
  }

  int setZero(int x) {
    auto save = get(x);
    nodes[x].val -= save.second;
    return save.second;
  }

  void add(int x, int val) {
    nodes[find(x)].delta += val;
  }
};

int main() {
  constexpr int p = 1'000'003;
  int n, m;
  std::cin >> n >> m;
  disjoint_set set(n);
  int zerg = 0;

  for (int k = 0; k < m; k++) {
    int command, i, j;
    int q;
    std::cin >> command >> i;
    i = (i + zerg) % n;
    switch (command) {
    case 1:
      set.add(i, 1);
      zerg = (30 * zerg + 239) % p;
      break;
    case 2:
      std::cin >> j;
      j = (j + zerg) % n;
      if (set.join(i, j))
        zerg = (13 * zerg + 11) % p;
      break;
    case 3:
      q = set.setZero(i);
      zerg = int((100500ull * zerg + q) % p);
      std::cout << q << std::endl;
      break;
    }
  }

  return 0;
}
