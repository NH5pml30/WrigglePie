/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <stack>
#include <algorithm>
#include <functional>

template<typename type>
class sparse_table {
 private:
  static int log2(int n) {
    int res = 0;
    while (n > 0) {
      n >>= 1;
      res++;
    }
    return res - 1;
  }

  std::vector<int> logs;
  std::function<type(const type &, const type &)> func;
  std::vector<std::vector<type>> f;

 public:
  sparse_table(const std::vector<type> &a,
               std::function<type(const type &, const type &)> func)
      : logs(a.size() + 1), func(func), f(a.size()) {
    int n = (int)a.size();
    logs[1] = 0;
    for (int i = 2; i <= n; i++) logs[i] = logs[i / 2] + 1;

    for (int i = 0; i < n; i++) {
      f[i].resize(logs[n - i] + 1);
      f[i][0] = a[i];
    }

    for (int k = 1, pow_k_minus_1 = 1; k <= logs[n]; k++, pow_k_minus_1 *= 2)
      for (int l = 0; l < n && k < logs[n - l] + 1; l++)
        f[l][k] = func(f[l][k - 1], f[l + pow_k_minus_1][k - 1]);
  }

  type eval(int l, int r) {
    int k = logs[r - l];
    return func(f[l][k], f[r - (1 << k)][k]);
  }
};

struct node {
  static std::vector<node> storage;
  std::vector<int> children;
  int in = -1;

  void dfs(int id, int depth, std::vector<std::pair<int, int>> &range) {
    if (in == -1) in = (int)range.size();
    range.push_back({depth, id});
    for (auto c : children) {
      storage[c].dfs(c, depth + 1, range);
      range.push_back({depth, id});
    }
  }
};

std::vector<node> node::storage;

unsigned clog2(unsigned x) {
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x++;

  unsigned res = 0;
  for (; x > 0; x >>= 1, res++) {
  }
  return res - 1;
}

int main() {
  int n, m;
  std::cin >> n >> m;

  auto &nodes = node::storage;

  nodes.resize(n);

  std::vector<std::vector<int>> children(n);

  for (int i = 0; i < n - 1; i++) {
    int pi;
    std::cin >> pi;
    nodes[pi].children.push_back(i + 1);
  }

  std::vector<std::pair<int, int>> range;
  range.reserve(2 * n - 1);
  nodes[0].dfs(0, 0, range);

  sparse_table<std::pair<int, int>> rmq(range,
    [](const std::pair<int, int> &l, const std::pair<int, int> &r) {
        return l.first < r.first ? l : r;
    });

  int a1, a2;
  std::cin >> a1 >> a2;
  long long x, y, z;
  std::cin >> x >> y >> z;

  long long res = 0;
  int last_ans = 0;
  for (int i = 0; i < m; i++) {
    int first = (a1 + last_ans) % n, second = a2;

    int f_in = nodes[first].in, s_in = nodes[second].in;

    int ans = rmq.eval(std::min(f_in, s_in), std::max(f_in, s_in) + 1).second;
    int new_a1 = (int)((x * a1 + y * a2 + z) % n),
        new_a2 = (int)((x * a2 + y * new_a1 + z) % n);
    a1 = new_a1;
    a2 = new_a2;
    res += ans;
    last_ans = ans;
  }

  std::cout << res << std::endl;

  return 0;
}
