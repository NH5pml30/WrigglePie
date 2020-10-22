/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>

class painted_graph {
 private:
  struct NODE_DATA {
   private:
    static NODE_DATA * in_attach(NODE_DATA *X, NODE_DATA *Y) {
      if (X == nullptr)
        return Y;
      if (Y == nullptr)
        return X;

      if (X->rank > Y->rank) {
        Y->parent = X->index;
        return X;
      } else {
        X->parent = Y->index;
        if (X->rank == Y->rank)
          Y->rank++;
        return Y;
      }
    }

   public:
    int index, parent, rank;
    bool color;
    NODE_DATA *other = nullptr;

    NODE_DATA(int index) :
      index(index), parent(index), rank(index), color(false) {
    }

    NODE_DATA() {
    }

    static void attach(NODE_DATA *X, NODE_DATA *Y) {
      if (X->color == Y->color) {
        Y->color ^= 1;
        if (Y->other != nullptr)
          Y->other->color ^= 1;
      }

      NODE_DATA *parent = in_attach(X, Y->other);
      parent->other = in_attach(X->other, Y);
      if (parent->other != nullptr)
        parent->other->other = parent;
    }
  };

  std::vector<NODE_DATA> nodes;

 public:
  painted_graph(int n) : nodes(n) {
    for (int i = 0; i < n; i++)
      nodes[i] = NODE_DATA(i);
  }

  int find(int x) {
    if (nodes[x].parent == x)
      return x;
    return nodes[x].parent = find(nodes[x].parent);
  }

  void join(int x, int y) {
    x = find(x), y = find(y);
    if (x == y)
      return;

    NODE_DATA::attach(&nodes[x], &nodes[y]);
  }

  bool sameColors(int x, int y) {
    return nodes[find(x)].color == nodes[find(y)].color;
  }
};

int main() {
  int n, m, shift = 0;
  std::cin >> n >> m;
  painted_graph graph(n);

  for (int i = 0; i < m; i++) {
    int command, a, b;
    std::cin >> command >> a >> b;
    int x = (a + shift) % n, y = (b + shift) % n;
    if (command == 0) {
      graph.join(x, y);
    } else {
      if (graph.sameColors(x, y)) {
        std::cout << "YES" << std::endl;
        shift = (shift + 1) % n;
      } else {
        std::cout << "NO" << std::endl;
      }
    }
  }
  return 0;
}
