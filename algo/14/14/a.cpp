/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  constexpr int MAX = 20'000'000;
  std::vector<bool> not_prime(MAX + 1);
  for (int i = 2; i <= MAX; i++)
    if (!not_prime[i] && (int64_t)i * i <= MAX)
      for (int j = i * i; j <= MAX; j += i)
        not_prime[j] = true;

  int n;
  std::cin >> n;
  for (int i = 0; i < n; i++) {
    int a;
    std::cin >> a;
    std::cout << (not_prime[a] ? "NO" : "YES") << '\n';
  }
  return 0;
}

