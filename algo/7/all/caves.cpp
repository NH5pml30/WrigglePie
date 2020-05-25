/* Kholiavin Nikolai, M3138 */
#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <set>
#include <vector>
#include <stack>
#include <memory>

template <typename type>
class monoid {
 private:
  std::function<type(const type &, const type &)> func;
  type identity_element;

 public:
  monoid() {}
  monoid(const std::function<type(const type &, const type &)> &func,
         type identity_element)
      : func(func), identity_element(std::move(identity_element)) {}

  type operator()(const type &lhs, const type &rhs) const {
    if (lhs == identity_element) return rhs;
    if (rhs == identity_element) return lhs;
    return func(lhs, rhs);
  }

  const type &identity() const { return identity_element; }
};

template <typename type>
class segment_tree {
 private:
  std::vector<type> nodes;
  int size;
  monoid<type> func;

  static int to_power2(int x) {
    x--;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    return x + 1;
  }

 private:
  void set(int i, const type &val, int x, int lx, int rx) {
    if (rx - lx == 1) {
      nodes[x] = val;
      return;
    }
    int m = (lx + rx) / 2;
    if (i < m)
      set(i, val, 2 * x + 1, lx, m);
    else
      set(i, val, 2 * x + 2, m, rx);
    nodes[x] = func(nodes[2 * x + 1], nodes[2 * x + 2]);
  }

  type apply(int l, int r, int x, int lx, int rx) {
    if (l >= rx || lx >= r) return func.identity();
    if (lx >= l && rx <= r) return nodes[x];
    int m = (lx + rx) / 2;
    return func(apply(l, r, 2 * x + 1, lx, m), apply(l, r, 2 * x + 2, m, rx));
  }

 public:
  segment_tree(const std::vector<type> &a, const monoid<type> &func)
      : func(func) {
    int pow2 = to_power2((int)a.size());
    size = pow2;
    nodes.resize(pow2 - 1);
    std::copy(a.begin(), a.end(), std::back_inserter(nodes));
    nodes.resize(pow2 * 2 - 1, func.identity());
    int begin = pow2 - 1;
    pow2 /= 2;
    begin -= pow2;
    for (; pow2 != 0; begin -= (pow2 /= 2))
      for (int i = 0; i < pow2; i++)
        nodes[begin + i] =
            func(nodes[2 * (begin + i) + 1], nodes[2 * (begin + i) + 2]);
  }

  void set(int at, const type &val) { set(at, val, 0, 0, size); }

  type apply(int l, int r) { return apply(l, r, 0, 0, size); }
};

struct node {
  static monoid<int> max;

  struct heavy_path {
    node *top;
    segment_tree<int> values;
    int len;

    heavy_path(node *top, const std::vector<int> &vals)
        : top(top), values(vals, max), len((int)vals.size()) {}
  };

  int value = 0;
  node *parent;
  int depth;
  std::vector<node *> children;
  std::shared_ptr<heavy_path> path;
  int path_begin;

  int subtree_size = 0;

  node() : parent(this) {}

  node(node *parent) : parent(parent) {}

  std::shared_ptr<heavy_path> decompose(std::vector<int> &vals, node *top,
                                        int depth = 0) {
    this->depth = depth;
    if (top == nullptr) top = this;
    vals.push_back(value);
    int top_len = (int)vals.size();

    bool found_heavy = false;
    for (auto child : children) {
      if (2 * child->subtree_size >= subtree_size) {
        found_heavy = true;
        path = child->decompose(vals, top, depth + 1);
        path_begin = path->len - top_len;
      } else {
        std::vector<int> empty;
        child->decompose(empty, nullptr, depth + 1);
      }
    }

    if (!found_heavy) {
      path = std::make_shared<heavy_path>(top, vals);
      path_begin = 0;
    }
    return path;
  }

  static node *LCA(node *u, node *v) {
    while (u->path != v->path) {
      node *old_u = u, *old_v = v;

      node *next_u = u == u->path->top ? u->parent : u->path->top,
           *next_v = v == v->path->top ? v->parent : v->path->top;

      if (next_u->depth > next_v->depth)
        u = next_u;
      else
        v = next_v;

      if (old_u == u && old_v == v) {
        u = next_u;
        v = next_v;
      }
    }
    return u->depth < v->depth ? u : v;
  }

  void add(int val) {
    path->values.set(path_begin, value += val);
  }

  static int count_subtree(node *p, node *c, bool count_parent) {
    int res = std::numeric_limits<int>::min();
    while (c->path != p->path) {
      res = std::max(res, c->path->values.apply(c->path_begin, c->path->len));
      c = c->path->top->parent;
    }
    return std::max(res, c->path->values.apply(c->path_begin, p->path_begin + count_parent));
  }

  static int count(node *u, node *v) {
    node *lca = LCA(u, v);
    return std::max(count_subtree(lca, u, true),
                    count_subtree(lca, v, false));
  }
};

monoid<int> node::max =
    monoid<int>([](auto x, auto y) { return std::max(x, y); },
                std::numeric_limits<int>::min());


int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n;
  std::cin >> n;

  std::vector<std::vector<int>> adjoint(n);

  for (int i = 0; i < n - 1; i++) {
    int u, v;
    std::cin >> u >> v;
    u--, v--;
    adjoint[u].push_back(v);
    adjoint[v].push_back(u);
  }

  std::vector<node *> nodes(n);
  std::stack<std::tuple<int, int, bool>> stack;
  for (stack.push({0, -1, true}); !stack.empty();) {
    auto[v, p, in] = stack.top();
    if (in) {
      nodes[v] = p == -1 ? new node() : new node(nodes[p]);
      if (p != -1) nodes[p]->children.push_back(nodes[v]);
      std::get<2>(stack.top()) = false;
      for (auto child : adjoint[v])
        if (child != p) stack.push({child, v, true});
    } else {
      nodes[v]->subtree_size = 1;
      for (auto child : adjoint[v])
        if (child != p) nodes[v]->subtree_size += nodes[child]->subtree_size;
      stack.pop();
    }
  }

  std::vector<int> empty;
  nodes[0]->decompose(empty, nullptr);

  int m;
  std::cin >> m;
  for (int i = 0; i < m; i++) {
    char command;
    int u, v;
    std::cin >> command >> u >> v;
    u--;

    switch (command) {
      case 'I':
        nodes[u]->add(v);
        break;
      case 'G':
        v--;
        std::cout << node::count(nodes[u], nodes[v]) << '\n';
        break;
      default:
        break;
    }
  }

  std::cout.flush();

  for (auto node : nodes) delete node;

  return 0;
}
