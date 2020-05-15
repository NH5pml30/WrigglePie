/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>

int log2(int n) {
  int res = 0;
  while (n > 0) {
    n >>= 1;
    res++;
  }
  return res - 1;
}

int main() {
  int n, m, a1;
  std::cin >> n >> m >> a1;

  int ui, vi;
  std::cin >> ui >> vi;

  std::vector<int> a(n);
  a[0] = a1;
  for (int i = 1; i < n; i++)
    a[i] = (23 * a[i - 1] + 21'563) % 16'714'589;

  std::vector<std::vector<int>> f(n);

  std::vector<int> logs(n + 1);
  logs[1] = 0;
  for (int i = 2; i <= n; i++)
    logs[i] = logs[i / 2] + 1;

  for (int i = 0; i < n; i++) {
    f[i].resize(logs[n - i] + 1);
    f[i][0] = a[i];
  }

  for (int k = 1, pow_k_minus_1 = 1; k <= logs[n]; k++, pow_k_minus_1 *= 2)
    for (int l = 0; l < n && k < logs[n - l] + 1; l++)
      f[l][k] = std::min(f[l][k - 1], f[l + pow_k_minus_1][k - 1]);

  int ans = 0;
  for (int i = 0; i < m; i++) {
    int
      l = std::min(ui - 1, vi - 1),
      r = std::max(ui - 1, vi - 1) + 1,
      k = logs[r - l];

    ans = std::min(f[l][k], f[r - (1 << k)][k]);
    if (i != m - 1) {
      ui = ((17 * ui + 751 + ans + 2 * (i + 1)) % n) + 1;
      vi = ((13 * vi + 593 + ans + 5 * (i + 1)) % n) + 1;
    }
  }

  std::cout << ui << ' ' << vi << ' ' << ans << std::endl;
  return 0;
}
