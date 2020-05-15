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

  type apply(int l, int r) {
    l += size - 1;
    r += size - 2;

    type ans_l = func.identity(), ans_r = func.identity();
    while (r > l) {
      if (l % 2 == 0)
        ans_l = func(ans_l, nodes[l]);
      l /= 2;
      if (r % 2 == 1)
        ans_r = func(nodes[r], ans_r);
      r = (r - 2) / 2;
    }
    if (l == r)
      ans_l = func(ans_l, nodes[l]);
    return func(ans_l, ans_r);
  }
};

struct matrix_mod {
  int a[2][2];
  static int r;

  static int mod(int x) {
    return (x % r) * (x < 0 ? -1 : 1);
  }

  matrix_mod() {}

  matrix_mod(int a00, int a01, int a10, int a11) {
    a[0][0] = mod(a00);
    a[0][1] = mod(a01);
    a[1][0] = mod(a10);
    a[1][1] = mod(a11);
  }

  bool operator==(const matrix_mod &other) const {
    return a[0][0] == other.a[0][0] &&
           a[0][1] == other.a[0][1] &&
           a[1][0] == other.a[1][0] &&
           a[1][1] == other.a[1][1];
  }

  static matrix_mod mult(const matrix_mod &lhs, const matrix_mod &rhs) {
    return matrix_mod(lhs.a[0][0] * rhs.a[0][0] + lhs.a[0][1] * rhs.a[1][0],
                      lhs.a[0][0] * rhs.a[0][1] + lhs.a[0][1] * rhs.a[1][1],
                      lhs.a[1][0] * rhs.a[0][0] + lhs.a[1][1] * rhs.a[1][0],
                      lhs.a[1][0] * rhs.a[0][1] + lhs.a[1][1] * rhs.a[1][1]);
  }

  static matrix_mod identity() {
    return matrix_mod(1, 0, 0, 1);
  }
};
int matrix_mod::r = 0;

std::istream & operator>>(std::istream &in, matrix_mod &matrix) {
  int a00, a01, a10, a11;
  in >> a00 >> a01 >> a10 >> a11;
  matrix = matrix_mod(a00, a01, a10, a11);
  return in;
}

std::ostream & operator<<(std::ostream &out, const matrix_mod &matrix) {
  return out << matrix.a[0][0] << ' ' << matrix.a[0][1] << '\n' <<
                matrix.a[1][0] << ' ' << matrix.a[1][1] << '\n';
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int r, n, m;
  std::cin >> r >> n >> m;
  matrix_mod::r = r;
  std::vector<matrix_mod> a(n);
  for (int i = 0; i < n; i++)
    std::cin >> a[i];

  monoid<matrix_mod> mult = monoid<matrix_mod>(
    matrix_mod::mult,
    matrix_mod::identity());

  segment_tree<matrix_mod> st(a, mult);

  for (int i = 0; i < m; i++) {
    int l, r;
    std::cin >> l >> r;
    l--, r--;
    std::cout << st.apply(l, r + 1) << '\n';
  }
  std::cout.flush();
  return 0;
}
