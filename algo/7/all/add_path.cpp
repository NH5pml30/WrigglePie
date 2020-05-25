/* Kholiavin Nikolai, M3138 */
#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
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

template <typename type, typename ranged_op>
class segment_tree {
 private:
  std::vector<type> nodes;
  std::vector<ranged_op> pending_ops;
  int size;
  monoid<type> func;
  monoid<ranged_op> op_compose;
  std::function<type(const type &, const ranged_op &)> op_apply;

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
  void propagate_(int x, int lx, int rx) {
    if (rx - lx == 1) return;
    if (pending_ops[x] == op_compose.identity()) return;
    apply_(2 * x + 1, pending_ops[x]);
    apply_(2 * x + 2, pending_ops[x]);
    pending_ops[x] = op_compose.identity();
  }

  void apply_(int x, const ranged_op &op) {
    nodes[x] = op_apply(nodes[x], op);
    pending_ops[x] = op_compose(pending_ops[x], op);
  }

  void apply_(int l, int r, const ranged_op &op, int x, int lx, int rx) {
    propagate_(x, lx, rx);
    if (l >= rx || lx >= r) return;
    if (lx >= l && rx <= r) {
      apply_(x, op);
      return;
    }

    int m = (lx + rx) / 2;
    apply_(l, r, op, 2 * x + 1, lx, m);
    apply_(l, r, op, 2 * x + 2, m, rx);
    nodes[x] = func(nodes[2 * x + 1], nodes[2 * x + 2]);
  }

  type eval_(int l, int r, int x, int lx, int rx) {
    propagate_(x, lx, rx);
    if (l >= rx || lx >= r) return func.identity();
    if (lx >= l && rx <= r) return nodes[x];
    int m = (lx + rx) / 2;
    return func(eval_(l, r, 2 * x + 1, lx, m), eval_(l, r, 2 * x + 2, m, rx));
  }

 public:
  segment_tree(
      const std::vector<type> &a, const monoid<type> &func,
      const monoid<ranged_op> &op_compose,
      const std::function<type(const type &, const ranged_op &)> &op_apply)
      : func(func), op_compose(op_compose), op_apply(op_apply) {
    int pow2 = to_power2((int)a.size());
    size = pow2;

    pending_ops.resize(2 * pow2 - 1, op_compose.identity());

    nodes.reserve(2 * pow2 - 1);
    nodes.resize(pow2 - 1);
    std::copy(a.begin(), a.end(), std::back_inserter(nodes));
    nodes.resize(2 * pow2 - 1, func.identity());
    int begin = pow2 - 1;
    pow2 /= 2;
    begin -= pow2;
    for (; pow2 != 0; begin -= (pow2 /= 2))
      for (int i = 0; i < pow2; i++)
        nodes[begin + i] =
            func(nodes[2 * (begin + i) + 1], nodes[2 * (begin + i) + 2]);
  }

  void apply(int l, int r, const ranged_op &op) {
    apply_(l, r, op, 0, 0, size);
  }

  type eval(int l, int r) { return eval_(l, r, 0, 0, size); }
};

struct node {
  static monoid<long long> sum;
  static std::function<long long(const long long &, const long long &)> adder;

  struct heavy_path {
    node *top;
    segment_tree<long long, long long> values;
    int len;

    heavy_path(node *top, const std::vector<long long> &vals)
        : top(top), values(vals, sum, sum, adder), len((int)vals.size()) {}
  };

  int value = 0;
  node *parent;
  int depth;
  std::vector<node *> children;
  std::shared_ptr<heavy_path> path;
  int path_begin;

  int subtree_size = 0;

  node() : parent(this) {}

  node(node *parent) : parent(parent) {
  }

  std::shared_ptr<heavy_path> decompose(std::vector<long long> &vals, node *top, int depth = 0) {
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
        std::vector<long long> empty;
        child->decompose(empty, nullptr, depth + 1);
      }
    }

    if (!found_heavy) {
      path = std::make_shared<heavy_path>(top, vals);
      path_begin = 0;
    }
    return path;
  }

  static node * LCA(node *u, node *v) {
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

  long long count() {
    return path->values.eval(path_begin, path_begin + 1);
  }

  static void add_subtree(node *p, node *c, long long val, bool count_parent) {
    while (c->path != p->path) {
      c->path->values.apply(c->path_begin, c->path->len, val);
      c = c->path->top->parent;
    }
    c->path->values.apply(c->path_begin, p->path_begin + count_parent, val);
  }

  static void add(node *u, node *v, long long val) {
    node *lca = LCA(u, v);
    add_subtree(lca, u, val, true);
    add_subtree(lca, v, val, false);
  }
};

std::function<long long(const long long &, const long long &)> node::adder =
    [](const auto &x, const auto &y) -> auto {
  return x + y;
};
monoid<long long> node::sum = monoid<long long>(adder, 0);

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

  std::vector<long long> empty;
  nodes[0]->decompose(empty, nullptr);

  int m;
  std::cin >> m;
  for (int i = 0; i < m; i++) {
    char command;
    int v;
    std::cin >> command >> v;
    v--;
    int u, d;

    switch (command) {
      case '+':
        std::cin >> u >> d;
        u--;
        node::add(nodes[v], nodes[u], d);
        break;
      case '?':
        std::cout << nodes[v]->count() << '\n';
        break;
      default:
        break;
    }
  }

  std::cout.flush();

  for (auto node : nodes) delete node;

  return 0;
}
