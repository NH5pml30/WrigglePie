/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <cassert>
#include <unordered_map>

int log2(int n) {
  int res = 0;
  while (n > 0) {
    n >>= 1;
    res++;
  }
  return res - 1;
}

template<typename type>
struct sparse_table_2d {
 public:
  using func_t = std::function<type (const type &, const type &)>;
  func_t func;
  std::vector<std::vector<std::vector<std::vector<type>>>> f;
  static std::vector<int> logs;

  sparse_table_2d() { }

  sparse_table_2d(const func_t &func, const std::vector<std::vector<type>> &a) :
    func(func), f(a.size()) {
    int n = (int)a.size(), m = (int)a[0].size(), max = std::max(n, m);
    if ((int)logs.size() <= max) {
      logs.reserve(max + 1);
      for (int i = (int)logs.size(); i <= max; i++)
        logs.push_back(logs[i / 2] + 1);
    }

    for (int i = 0; i < n; i++) {
      f[i].resize(m);
      for (int j = 0; j < m; j++) {
        f[i][j].resize(logs[n - i] + 1);
        for (int k = 0; k < logs[n - i] + 1; k++) {
          f[i][j][k].resize(logs[m - j] + 1);
          f[i][j][k][0] = a[i][j];
        }
      }
    }

    for (int k = 1, pow_k_minus_1 = 1; k <= logs[n]; k++, pow_k_minus_1 *= 2)
      for (int i = 0; i < n && k < logs[n - i] + 1; i++)
        for (int j = 0; j < m; j++)
          f[i][j][k][0] = func(f[i][j][k - 1][0], f[i + pow_k_minus_1][j][k - 1][0]);

    for (int k2 = 1, pow_k_minus_1 = 1; k2 <= logs[m]; k2++, pow_k_minus_1 *= 2)
      for (int j = 0; j < m && k2 < logs[m - j] + 1; j++)
        for (int k1 = 0; k1 <= logs[n]; k1++)
          for (int i = 0; i < n && k1 < logs[n - i] + 1; i++)
            f[i][j][k1][k2] = func(f[i][j][k1][k2 - 1], f[i][j + pow_k_minus_1][k1][k2 - 1]);
  }

  type RMQ(int x1, int y1, int x2, int y2) const {
    int k1 = logs[y2 - y1], k2 = logs[x2 - x1];
    type
      ans1 = f[y1][x1][k1][k2],
      ans2 = f[y2 - (1 << k1)][x1][k1][k2],
      ans3 = f[y1][x2 - (1 << k2)][k1][k2],
      ans4 = f[y2 - (1 << k1)][x2 - (1 << k2)][k1][k2];
    return func(func(ans1, ans2), func(ans3, ans4));
  }
};

template<typename type>
  std::vector<int> sparse_table_2d<type>::logs = {0, 0};

struct rect {
  int x1, y1, x2, y2;

  rect() {}
  rect(int x1, int y1, int x2, int y2) : x1(x1), y1(y1), x2(x2), y2(y2) {
    if (x1 > x2) std::swap(this->x1, this->x2);
    if (y1 > y2) std::swap(this->y1, this->y2);
  }

  long long area() const { return (long long)(x2 - x1) * (y2 - y1); }

  static rect intersect(const rect &lhs, const rect &rhs) {
    int
      x1 = std::max(lhs.x1, rhs.x1),
      y1 = std::max(lhs.y1, rhs.y1),
      x2 = std::max(std::min(lhs.x2, rhs.x2), x1),
      y2 = std::max(std::min(lhs.y2, rhs.y2), y1);
    return rect(x1, y1, x2, y2);
  }
};

class random_generator {
 private:
  static const int mod = 1'000'000'007;
  int A, B;
  int v;

 public:
  random_generator(int A, int B, int v0) : A(A), B(B), v(v0) {}

  int next() { return v = (int)(((long long)A * v + B) % mod); }
};

int main() {
  constexpr int mod = 1'000'000'007;

  int N, M;
  std::cin >> N >> M;
  std::vector<std::vector<rect>> t(N);
  for (int i = 0; i < N; i++) {
    t[i].reserve(M);
    for (int j = 0; j < M; j++) {
      int x1, y1, x2, y2;
      std::cin >> x1 >> y1 >> x2 >> y2;
      t[i].push_back(rect(x1, y1, x2, y2));
    }
  }

  sparse_table_2d<rect> table(rect::intersect, t);

  int Q, A, B, v0;
  std::cin >> Q >> A >> B >> v0;
  random_generator gen(A, B, v0);

  int result = 0;
  for (int k = 0; k < Q; k++) {
    int
      r1 = gen.next() % N,
      c1 = gen.next() % M,
      r2 = gen.next() % N,
      c2 = gen.next() % M;
    if (r1 > r2) std::swap(r1, r2);
    if (c1 > c2) std::swap(c1, c2);
    result = (int)((result + table.RMQ(c1, r1, c2 + 1, r2 + 1).area()) % mod);
  }

  std::cout << result << std::endl;

  return 0;
}
