/* Kholiavin Nikolai, M3138 */
#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <iterator>
#include <unordered_map>
#include <vector>

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

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  monoid<std::unordered_map<int, int>> merge_list = monoid<std::unordered_map<int, int>>(
      [](const std::unordered_map<int, int> &x, const std::unordered_map<int, int> &y) {
        std::unordered_map<int, int> res = x;
        for (auto el : y)
          res[el.first] += el.second;
        return res;
      },
      {});

  int n, m;
  std::cin >> n >> m;
  std::vector<std::unordered_map<int, int>> a(n);
  for (int i = 0; i < n; i++) {
    int x;
    std::cin >> x;
    a[i] = {{x, 1}};
  }

  segment_tree<std::unordered_map<int, int>> st(a, merge_list);

  for (int i = 0; i < m; i++) {
    int l, r;
    std::cin >> l >> r;
    l--, r--;
    std::unordered_map<int, int> numbers = st.apply(l, r + 1);
    long long res = 0;
    for (auto el : numbers)
      res += (long long)el.second * el.second * el.first;
    std::cout << res << '\n';
  }
  std::cout.flush();
  return 0;
}
