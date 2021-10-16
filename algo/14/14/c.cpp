/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <random>

bool stupid_is_prime(int x, const std::vector<int> &prime_gaps) {
  int upper = (int)sqrt(x), prime = 2;
  if (x % 2 == 0)
    return false;
  for (int gap : prime_gaps) {
    prime += gap;
    if (prime > upper)
      break;
    if (x % prime == 0)
      return false;
  }
  return true;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n, x;
  std::cin >> n >> x;

  int32_t h = 0;

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
        h = h * x + i;
      }
  }

  for (int block_begin = block_size; block_begin <= n; block_begin += block_size) {
    std::vector<bool> not_prime(block_size);
    for (auto prime : precount_primes) {
      for (int j = (block_begin + prime - 1) / prime * prime - block_begin; j < block_size;
           j += prime)
        not_prime[j] = true;
    }
    for (int i = 0; i < block_size && block_begin + i <= n; i++)
      if (!not_prime[i])
        h = h * x + block_begin + i;
  }

  std::cout << h << std::endl;
  return 0;
}

