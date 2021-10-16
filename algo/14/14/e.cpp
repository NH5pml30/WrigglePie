/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <random>
#include <tuple>
#include <cassert>

std::tuple<int, int, int> euclid_(int a, int b) {
  if (b == 0)
    return {a, 1, 0};
  else {
    auto [d, x_, y_] = euclid_(b, a % b);
    return {d, y_, x_ - a / b * y_};
  }
}

std::tuple<int, int, int> euclid(int a, int b) {
  if (a < b) {
    auto [d, x, y] = euclid_(b, a);
    return {d, y, x};
  } else {
    return euclid_(a, b);
  }
}

namespace modular {

int inv(int n, int mod) {
  auto [d, x, y] = euclid(n, mod);
  (void)y;
  assert(d == 1);
  if (x < 0)
    x += (-x + mod - 1) / mod * mod;
  return x % mod;
}

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

} // namespace modular

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n, e, C;
  std::cin >> n >> e >> C;

  int block_size = std::min(32'000, n + 1);
  std::vector<int> precount_primes;
  {
    std::vector<bool> not_prime(block_size);
    for (int i = 2; i < block_size; i++)
      if (!not_prime[i]) {
        if ((int64_t)i * i < block_size) {
          for (int j = i * i; j < block_size; j += i)
            not_prime[j] = true;
        }
        precount_primes.push_back(i);
      }
  }

  int p = 0, q = 0;
  for (auto prime : precount_primes)
    if (n % prime == 0) {
      p = prime, q = n / p;
      break;
    }
  int d = modular::inv(e, (p - 1) * (q - 1));
  std::cout << modular::pow(C, d, n) << std::endl;
  return 0;
}

