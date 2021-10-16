/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <random>

namespace modular {

template<typename type>
int last_bit(type x) {
  for (int i = sizeof(x) * CHAR_BIT - 1; i >= 0; i--)
    if (((x >> i) & 1) != 0)
      return i;
  return -1;
}

int64_t mul(int64_t a, int64_t b, int64_t mod) {
  int64_t res = 0;
  for (int i = last_bit(b); i >= 0; i--) {
    res = (res * 2) % mod;
    if (((b >> i) & 1) != 0)
      res = (res + a) % mod;
  }
  return res;
}

int64_t pow(int64_t b, int64_t p, int64_t mod) {
  int64_t res = 1;
  for (int i = last_bit(p); i >= 0; i--) {
    res = mul(res, res, mod);
    if (((p >> i) & 1) != 0)
      res = mul(b, res, mod);
  }
  return res;
}

}

bool miller_rabin_test(int64_t n, int T) {
  if (n < 2)
    return false;
  if (n == 2)
    return true;

  int64_t s = n - 1, k = 0;
  while (s % 2 == 0) {
    s /= 2;
    k++;
  }

  std::default_random_engine engine{};
  std::uniform_int_distribution<int64_t> distr(2, n - 1);

  for (int i = 0; i < T; i++) {
    int64_t x = distr(engine);
    int64_t y = modular::pow(x, s, n);
    if (y == 1)
      continue;
    if (modular::pow(y, int64_t{1} << k, n) != 1)
      return false;
    while (modular::mul(y, y, n) != 1)
      y = modular::mul(y, y, n);
    if (y != n - 1)
      return false;
  }
  return true;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n;
  std::cin >> n;

  for (int i = 0; i < n; i++) {
    int64_t a;
    std::cin >> a;
    std::cout << (miller_rabin_test(a, 10) ? "YES" : "NO") << '\n';
  }

  std::cout.flush();
  return 0;
}

