/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <stack>
#include <list>
#include <algorithm>

class disjoint_set {
 private:
  struct NODE_DATA {
    int index, parent, rank;
    int val = 0;

    NODE_DATA(int index) :
      index(index), parent(index), rank(index) {
    }

    NODE_DATA() {
    }

    static int attach(NODE_DATA &X, NODE_DATA &Y) {
      if (X.rank > Y.rank) {
        Y.parent = X.index;
        return X.index;
      } else {
        X.parent = Y.index;
        if (X.rank == Y.rank)
          Y.rank++;
        return Y.index;
      }
    }
  };

  std::vector<NODE_DATA> nodes;

  int find_update(int x) {
    if (nodes[x].parent == x)
      return x;
    return nodes[x].parent = find_update(nodes[x].parent);
  }

 public:
  disjoint_set(int n) : nodes(n) {
    int i = 0;
    for (auto &el : nodes)
      el = NODE_DATA(i++);
  }

  int find(int x) {
    return find_update(x);
  }

  int join(int x, int y) {
    x = find(x), y = find(y);
    if (x == y)
      return x;
    return NODE_DATA::attach(nodes[x], nodes[y]);
  }
};

int main() {
  struct node;
  struct query {
    int u, v;
    int ans;

    query(int u, int v) : u(u), v(v) {
    }
  };

  struct node {
    int id;
    int parent_id;
    std::vector<int> children;

    std::list<query *> related_queries;

    node(int id, int parent_id) : id(id), parent_id(parent_id) {
    }

    void add_child(int id) {
      children.push_back(id);
    }
  };

  std::vector<node> nodes = {node(0, -1)};
  std::list<query> queries;
  int k;
  std::cin >> k;

  for (int i = 0; i < k; i++) {
    std::string cmd;
    int a, b;
    std::cin >> cmd >> a >> b;
    a--, b--;
    if (cmd == "ADD") {
      nodes.push_back(node(b, a));
      nodes[a].add_child(b);
    } else {  // "GET"
      queries.push_back(query(a, b));
      if (a == b) {
        queries.back().ans = a;
      } else {
        nodes[a].related_queries.push_back(&queries.back());
        nodes[b].related_queries.push_back(&queries.back());
      }
    }
  }

  disjoint_set ds(nodes.size());
  std::vector<int> ds_root2anc(nodes.size(), 0);
  std::vector<bool> was(nodes.size());

  std::stack<std::pair<int, int>> stack;
  stack.push({0, -1});

  while (!stack.empty()) {
    auto[v, join_to] = stack.top();
    node *self = &nodes[v];
    if (!was[v]) {
      was[v] = true;
      ds_root2anc[v] = v;
      for (auto child : self->children)
        stack.push({child, v});
    } else {
      for (auto q = self->related_queries.begin(); q != self->related_queries.end();) {
        int u = (*q)->v == v ? (*q)->u : (*q)->v;
        node *other = &nodes[u];
        if (was[u]) {
          auto next = std::next(q);
          (*q)->ans = ds_root2anc[ds.find(u)];
          other->related_queries.erase(std::find(other->related_queries.begin(),
                                                 other->related_queries.end(), (query *)*q));
          self->related_queries.erase(q);
          q = next;
        } else {
          q++;
        }
      }
      if (join_to != -1)
        ds_root2anc[ds.join(v, join_to)] = join_to;
      stack.pop();
    }
  }

  for (auto q : queries) {
    std::cout << q.ans + 1 << std::endl;
  }

  return 0;
}
