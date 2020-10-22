/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <cstring>

int main() {
  int n;
  std::cin >> n;

  std::vector<int> a(n);
  for (int i = 0; i < n; i++)
    std::cin >> a[i];

  std::vector<int> f(n * n, 1);

  constexpr int mod = 1'000'000'000;
  for (int i = n - 1; i >= 0; i--) {
    memset(f.data() + i * n, 0, i * sizeof(int));
    for (int j = i + 1; j < n; j++)
      if (a[i] == a[j])
        f[i * n + j] = (f[(i + 1) * n + j] + f[i * n + j - 1] + 1) % mod;
      else
        f[i * n + j] = ((f[(i + 1) * n + j] + f[i * n + j - 1]) % mod +
                        mod - f[(i + 1) * n + j - 1]) % mod;
  }

  std::cout << f[0 * n + n - 1] << std::endl;

  return 0;
}
