/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <functional>
#include <fstream>

template<typename type>
  int firstGEX(const std::vector<type> &a, type x) {
    int L = -1, R = (int)a.size();

    while (L < R - 1) {
      int M = (L + R) / 2;
      if (a[M] >= x)
        R = M;
      else
        L = M;
    }

    return R;
  }

int main() {
  std::ifstream in("lis.in");
  std::ofstream out("lis.out");

  int n;
  in >> n;

  constexpr int
    neg_inf = std::numeric_limits<int>::min(),
    inf = std::numeric_limits<int>::max();

  struct INDEX_VAL {
    int val, index;

    INDEX_VAL(int val, int index) : val(val), index(index) {
    }

    bool operator>=(const INDEX_VAL &other) const {
      return val >= other.val;
    }
  };

  std::vector<int> a(n), p(n, -1);
  std::vector<INDEX_VAL> g(n + 1, {inf, -1});
  g[0] = {neg_inf, -1};

  for (int i = 0; i < n; i++) {
    in >> a[i];

    int place = firstGEX(g, {a[i], -1});
    g[place].val = a[i];
    g[place].index = i;
    p[i] = g[place - 1].index;
  }

  int max = firstGEX(g, {inf, -1}) - 1;
  std::vector<int> path;
  path.reserve(max);

  int cur = g[max].index;
  while (cur >= 0) {
    path.push_back(a[cur]);
    cur = p[cur];
  }

  out << max << std::endl;
  for (int i = max - 1; i >= 0; i--)
    out << path[i] << " ";
  out << std::endl;
  return 0;
}
