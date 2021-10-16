/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  constexpr int MAX = 1'000'000;
  std::vector<int> first_prime(MAX + 1, -1);
  for (int i = 2; i <= MAX; i++)
    if (first_prime[i] == -1 && (int64_t)i * i <= MAX)
      for (int j = i * i; j <= MAX; j += i)
        if (first_prime[j] == -1)
          first_prime[j] = i;

  int n;
  std::cin >> n;
  for (int i = 0; i < n; i++) {
    int a;
    std::cin >> a;
    while (a > 1) {
      int factor = first_prime[a] == -1 ? a : first_prime[a];
      std::cout << factor << ' ';
      a /= factor;
    }
    std::cout << '\n';
  }
  return 0;
}

