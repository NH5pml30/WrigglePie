/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <cassert>
#include <unordered_map>

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
struct set_add {
  enum class op_type {
    SET, ADD
  } t;
  type val;

  set_add(op_type t, type val) : t(t), val(val) {}
  set_add() {}

  bool operator==(const set_add &other) const {
    return t == other.t && val == other.val;
  }

  static set_add compose(const set_add &lhs, const set_add &rhs) {
    if (rhs.t == op_type::SET)
      return rhs;
    else if (lhs.t == op_type::SET)
      return set_add(op_type::SET, lhs.val + rhs.val);
    else
      return set_add(op_type::ADD, lhs.val + rhs.val);
  }

  static type apply(const type &lhs, const set_add &rhs) {
    if (rhs.t == op_type::ADD)
      return lhs + rhs.val;
    else
      return rhs.val;
  }

  static set_add identity;
};

template<typename type>
  set_add<type> set_add<type>::identity = set_add<type>(set_add<type>::op_type::ADD, (type)0);

int main() {
  using set_add_t = set_add<long long>;

  monoid<long long> minimum = monoid<long long>(
    [](const auto &x, const auto &y) { return std::min(x, y); },
    std::numeric_limits<long long>::min());
  monoid<set_add_t> applier = monoid<set_add_t>(
    set_add_t::compose,
    set_add_t::identity);

  int n;
  std::cin >> n;
  std::vector<long long> a(n);
  for (int i = 0; i < n; i++)
    std::cin >> a[i];

  segment_tree<long long, set_add_t> st(a, minimum, applier, set_add_t::apply);

  std::string command;
  while (std::cin >> command) {
    if (command == "min") {
      int i, j;
      std::cin >> i >> j;
      i--, j--;
      std::cout << st.eval(i, j + 1) << std::endl;
    } else if (command == "set") {
      int i, j;
      long long x;
      std::cin >> i >> j >> x;
      i--, j--;
      st.apply(i, j + 1, set_add_t(set_add_t::op_type::SET, x));
    } else {  // add
      int i, j;
      long long x;
      std::cin >> i >> j >> x;
      i--, j--;
      st.apply(i, j + 1, set_add_t(set_add_t::op_type::ADD, x));
    }
  }
  return 0;
}
