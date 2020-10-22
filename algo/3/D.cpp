/* Nikolai Kholiavin, M3138 */
#include <algorithm>
#include <initializer_list>
#include <iostream>
#include <vector>

class disjoint_set {
 public:
  struct SET_DATA {
    int num;
    int min, max;

    SET_DATA(int index) : num(1), min(index), max(index) {
    }

    SET_DATA() {}

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

    NODE_DATA() {}

    static void attach(NODE_DATA &X, NODE_DATA &Y) {
      if (X.rank > Y.rank) {
        Y.parent = X.index;
        X.data.update(Y.data);
      } else {
        X.parent = Y.index;
        if (X.rank == Y.rank) Y.rank++;
        Y.data.update(X.data);
      }
    }
  };

  std::vector<NODE_DATA> nodes;

 public:
  disjoint_set(int n) : nodes(n) {
    int i = 0;
    for (auto &el : nodes) el = NODE_DATA(i++);
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
  bool is_last_taken = false;

  for (int i = 0; i < n; i++) {
    int pi;
    std::cin >> pi;
    pi--;

    int place = set.get(pi).max;
    if (place == n - 1) {
      if (!is_last_taken)
        is_last_taken = true;
      else
        place = set.get(0).max;
    }

    if (place != n - 1)
      set.join(place, place + 1);
    std::cout << place + 1 << ' ';
  }
  std::cout << std::endl;

  return 0;
}
