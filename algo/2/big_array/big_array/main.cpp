/* Kholiavin Nikolai, M3138 */

#include <iostream>
#include <functional>
#include <algorithm>

constexpr long long MOD0 = 1'000'000'000'000'000'000;

long long Get(long long Index) {
  std::cout << "? " << Index << std::endl;
  long long bi;
  std::cin >> bi;
  return bi;
}

void Put(long long Index) {
  std::cout << "! " << Index << std::endl;
}

long long BinSearch(long long I1, long long I2, long long X,
                    std::function<long long(long long Index)> Get) {
  while (I1 <= I2) {
    long long M = (I1 + I2) / 2, bm = Get(M);
    if (bm - X > MOD0 / 2)
      bm -= MOD0;
    else if (X - bm > MOD0 / 2)
      bm += MOD0;

    if (bm < X)
      I1 = M + 1;
    else if (bm > X)
      I2 = M - 1;
    else
      return M;
  }
  return -1;
}

long long Find(long long X, int M, std::function<long long(long long Index)> Get) {
  long long
    mod = MOD0 - M,
    b_1 = Get(1), i1, i2;

  if (b_1 > X) {
    i1 = (X - b_1 + mod) % mod + 1;
    i2 = std::min(i1 + M, mod);
  } else {
    i2 = X - b_1 + 1;
    i1 = std::max(i2 - M, 1ll);
  }
  return BinSearch(i1, i2, X, Get);
}

int main() {
  long long x;
  int m;
  std::cin >> x >> m;

  Put(Find(x, m, Get));
  return 0;
}
