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

template<typename type>
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
    if (l >= rx || lx >= r)
      return func.identity();
    if (lx >= l && rx <= r)
      return nodes[x];
    int m = (lx + rx) / 2;
    return func(apply(l, r, 2 * x + 1, lx, m), apply(l, r, 2 * x + 2, m, rx));
  }

 public:
  segment_tree(const std::vector<type> &a, const monoid<type> &func) : func(func) {
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
        nodes[begin + i] = func(nodes[2 * (begin + i) + 1], nodes[2 * (begin + i) + 2]);
  }

  void set(int at, const type &val) {
    set(at, val, 0, 0, size);
  }

  type apply(int l, int r) {
    return apply(l, r, 0, 0, size);
  }
};

int main() {
  monoid<int> minimum = monoid<int>(
    [](const auto &x, const auto &y) -> auto { return std::min(x, y); },
    std::numeric_limits<int>::min());

  int n;
  std::cin >> n;
  std::vector<int> a(n);
  for (int i = 0; i < n; i++)
    std::cin >> a[i];

  segment_tree<int> st(a, minimum);

  std::string command;
  while (std::cin >> command) {
    if (command == "min") {
      int i, j;
      std::cin >> i >> j;
      i--, j--;
      std::cout << st.apply(i, j + 1) << std::endl;
    } else {
      int i, x;
      std::cin >> i >> x;
      i--;
      st.set(i, x);
    }
  }
  return 0;
}
