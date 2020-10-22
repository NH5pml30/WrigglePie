/* Nikolai Kholiavin, M3138 */
#include <fstream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cstring>

int GetNBits(int X) {
  int res = 0;
  while (X > 0)
    res += (X & 1), X >>= 1;
  return res;
}

int GetLastBit(int X) {
  int res = sizeof(X) * 8 - 1;
  for (; res >= 0; res--)
    if ((X >> res) & 1)
      break;
  return res;
}

int main() {
  std::string name = "salesman";
  std::ifstream in(name + ".in");
  std::ofstream out(name + ".out");

  int n, m;
  in >> n >> m;

  std::vector<int> matrix(n * n);

  for (int i = 0; i < m; i++) {
    int a, b, w;
    in >> a >> b >> w;
    a--, b--;
    matrix[a * n + b] = matrix[b * n + a] = w;
  }

  constexpr int inf = std::numeric_limits<int>::max();

  int pown = 1 << n;
  std::vector<int> f(pown * n, inf);
  for (int A = 1; A < pown; A++) {
    if (GetNBits(A) == 1) {
      f[A * n + GetLastBit(A)] = 0;
      continue;
    }

    for (int i = 0; i < n; i++)
      if ((A >> i) & 1) {
        int Awo = A ^ (1 << i);
        for (int j = 0; j < n; j++)
          if (matrix[j * n + i] > 0 && ((Awo >> j) & 1) &&
              f[Awo * n + j] != inf)
            f[A * n + i] = std::min(f[A * n + i], f[Awo * n + j] + matrix[j * n + i]);
      }
  }

  int min = inf;
  for (int i = 0; i < n; i++)
    min = std::min(min, f[(pown - 1) * n + i]);

  out << (min == std::numeric_limits<int>::max() ? -1 : min) << std::endl;

  return 0;
}
