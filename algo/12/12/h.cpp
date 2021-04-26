/* Kholiavin Nikolai, M3238 */
#include <iostream>
#include <string>
#include <vector>
#include <numeric>
#include <algorithm>
#include <functional>
#include <random>

int main() {
  uint64_t m = 1'000'000'007;
  std::default_random_engine eng;
  std::uniform_int_distribution<uint64_t> dist(2, m - 1);
  uint64_t x = dist(eng);

  std::string s;
  std::cin >> s;

  std::vector<uint64_t> hash;
  std::vector<uint64_t> xpows = {1};
  hash.reserve(s.size());
  xpows.reserve(s.size() + 1);
  uint64_t last_hash = 0, last_xpow = 1;
  for (auto ch : s) {
    hash.push_back(last_hash = (last_hash * x + ch) % m);
    xpows.push_back(last_xpow = (last_xpow * x) % m);
  }

  auto substr_hash = [&](int l, int r) {
    return ((int64_t)m +
            (int64_t)(hash[r - 1] - xpows[r - l] * (l == 0 ? 0 : hash[l - 1])) % (int64_t)m) %
           m;
  };

  int M;
  std::cin >> M;
  for (int i = 0; i < M; i++) {
    int a, b, c, d;
    std::cin >> a >> b >> c >> d;
    --a, --c;
    bool is_eq = true;
    if (b - a == d - c && substr_hash(a, b) == substr_hash(c, d)) {
      int n_checks = 100;
      std::uniform_int_distribution<int> dist2(0, b - a - 1);
      for (int j = 0; j < n_checks; j++) {
        int at = dist2(eng);
        if (s[a + at] != s[c + at]) {
          is_eq = false;
          break;
        }
      }
    } else {
      is_eq = false;
    }

    std::cout << (is_eq ? "Yes" : "No") << '\n';
  }

  std::cout.flush();
  return 0;
}
