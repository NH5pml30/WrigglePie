/* Nikolai Kholiavin, M3138 */
#include <algorithm>
#include <iostream>
#include <vector>

class disjoint_set {
 public:
  struct SET_DATA {
    int num;
    int min, max;

    SET_DATA(int index) : num(1), min(index), max(index) {}

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

    NODE_DATA(int index)
        : index(index), parent(index), rank(index), data(index) {}

    NODE_DATA() {}

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
  int n, m, k;
  std::cin >> n >> m >> k;
  disjoint_set set(n);

  struct OPER {
    enum class type { ASK, CUT } t;
    int u, v;

    OPER(type t, int u, int v) : t(t), u(u), v(v) {}

    OPER() {}
  };

  std::vector<OPER> opers(k);

  for (int i = 0; i < m; i++) {
    int u, v;
    std::cin >> u >> v;
  }

  int asks = 0;
  for (int i = 0; i < k; i++) {
    std::string command;
    int u, v;
    std::cin >> command >> u >> v;
    u--, v--;
    if (command == "cut") {
      opers[i] = OPER(OPER::type::CUT, u, v);
    } else {
      asks++;
      opers[i] = OPER(OPER::type::ASK, u, v);
    }
  }

  std::vector<bool> res(asks);

  for (int i = k - 1; i >= 0; i--)
    switch (opers[i].t) {
      case OPER::type::ASK:
        res[--asks] = set.find(opers[i].u) == set.find(opers[i].v);
        break;
      case OPER::type::CUT:
        set.join(opers[i].u, opers[i].v);
        break;
    }

  for (auto c : res)
    std::cout << (c ? "YES" : "NO") << std::endl;
  return 0;
}
