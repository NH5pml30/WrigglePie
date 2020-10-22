/* Nikolai Kholiavin, M3138 */
#include <vector>
#include <iostream>
#include <initializer_list>
#include <algorithm>

class disjoint_set {
 public:
  struct SET_DATA {
    int num;
    int min, max;

    SET_DATA(int index) :
      num(1), min(index), max(index) {
    }

    SET_DATA() {
    }

    void update(SET_DATA childData) {
      num += childData.num;
      min = std::min(min, childData.min);
      max = std::max(max, childData.max);
    }
  };

 private:
  struct NODE_DATA {
    int index, parent, rank;
    SET_DATA data;

    NODE_DATA(int index) :
      index(index), parent(index), rank(index), data(index) {
    }

    NODE_DATA() {
    }

    static void attach(NODE_DATA &X, NODE_DATA &Y) {
      if (X.rank > Y.rank) {
        Y.parent = X.index;
        X.data.update(Y.data);
      } else {
        X.parent = Y.index;
        if (X.rank == Y.rank)
          Y.rank++;
        Y.data.update(X.data);
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

  SET_DATA get(int x) {
    return nodes[find(x)].data;
  }

  void join(int x, int y) {
    x = find(x), y = find(y);
    if (x == y)
      return;

    NODE_DATA::attach(nodes[x], nodes[y]);
  }
};

int main() {
  int n;
  std::cin >> n;

  disjoint_set set(n);

  while (!std::cin.eof()) {
    std::string command;
    std::cin >> command;
    if (command == "union") {
      int x, y;
      std::cin >> x >> y;
      set.join(x - 1, y - 1);
    } else if (command == "get") {
      int x;
      std::cin >> x;
      auto data = set.get(x - 1);
      std::cout << data.min + 1 << ' ' << data.max + 1 << ' ' << data.num << std::endl;
    }
  }

  return 0;
}
