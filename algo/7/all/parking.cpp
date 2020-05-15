/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <cassert>
#include <unordered_map>
#include <optional>

template<typename type>
class monoid {
 private:
  std::function<type(const type &, const type &)> func;
  type identity_element;

 public:
  monoid() {}
  monoid(const std::function<type(const type &, const type &)> &func, type identity_element) :
    func(func), identity_element(std::move(identity_element)) {
  }

  type operator()(const type &lhs, const type &rhs) const {
    if (lhs == identity_element)
      return rhs;
    if (rhs == identity_element)
      return lhs;
    return func(lhs, rhs);
  }

  const type & identity() const {
    return identity_element;
  }
};

template<typename type, typename ranged_op>
class segment_tree {
 protected:
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
    if (l >= rx || lx >= r)
      return func.identity();
    if (lx >= l && rx <= r)
      return nodes[x];
    int m = (lx + rx) / 2;
    return func(eval_(l, r, 2 * x + 1, lx, m), eval_(l, r, 2 * x + 2, m, rx));
  }

 public:
  segment_tree(const std::vector<type> &a, const monoid<type> &func,
               const monoid<ranged_op> &op_compose,
               const std::function<type(const type &, const ranged_op &)> &op_apply) :
    func(func), op_compose(op_compose), op_apply(op_apply) {
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
        nodes[begin + i] = func(nodes[2 * (begin + i) + 1], nodes[2 * (begin + i) + 2]);
  }

  void apply(int l, int r, const ranged_op &op) {
    apply_(l, r, op, 0, 0, size);
  }

  type eval(int l, int r) {
    return eval_(l, r, 0, 0, size);
  }
};

template<typename type>
class segment_tree_set_find : private segment_tree<type, std::optional<type>> {
 private:
  using op_t = std::optional<type>;
  using base_t = segment_tree<type, op_t>;

  int lower_bound_(const type &val, int x, int lx, int rx) {
    base_t::propagate_(x, lx, rx);
    if (rx - lx == 1) {
      if (!(base_t::nodes[x] < val)) return lx;
      return base_t::size;
    }

    int m = (lx + rx) / 2;
    if (base_t::nodes[2 * x + 1] < val)
      return lower_bound_(val, 2 * x + 2, m, rx);
    else
      return lower_bound_(val, 2 * x + 1, lx, m);
  }

 public:
  segment_tree_set_find(const std::vector<type> &a) :
    segment_tree<type, std::optional<type>>(a,
      monoid<type>([](auto &lhs, auto &rhs) { return std::max(lhs, rhs); },
                   std::numeric_limits<type>::min()),
      monoid<op_t>([](const op_t &lhs, const op_t &rhs) {
        return lhs ? rhs ? rhs.value() : lhs.value() : rhs.value();
      }, {}),
      [](const type &lhs, const op_t &rhs) { return rhs ? rhs.value() : lhs; }) {}

  type get(int at) {
    return base_t::eval(at, at + 1);
  }

  int lower_bound(const type &x) {
    return lower_bound_(x, 0, 0, base_t::size);
  }

  void set(int l, int r, const type &val) {
    return base_t::apply(l, r, val);
  }
};

int main() {
  int n, m;
  std::cin >> n >> m;

  std::vector<int> a(n);
  for (int i = 0; i < n; i++)
    a[i] = i;
  segment_tree_set_find<int> st(a);

  for (int i = 0; i < m; i++) {
    std::string command;
    std::cin >> command;
    if (command == "enter") {
      int x;
      std::cin >> x;
      x--;
      int empty = st.get(x);
      if (empty >= n) empty = st.get(0);
      std::cout << empty + 1 << std::endl;
      int first = st.lower_bound(empty), last = std::min(st.lower_bound(empty + 1), n);
      st.set(first, last, empty == n - 1 ? n : st.get(empty + 1));
    } else {  // exit
      int x;
      std::cin >> x;
      x--;
      int first = st.lower_bound(x + 1);
      st.set(first, x + 1, x);
    }
  }
  return 0;
}
