/* Kholiavin Nikolai, M3138 */

#include <iostream>
#include <vector>
#include <functional>

int * BinarySearch(int *L, int *R, std::function<bool(int Val)> Fun) {
  int *M;
  while (L < R - 1) {
    M = L + (R - L) / 2;
    if (Fun(*M))
      R = M;
    else
      L = M;
  }

  return L;
}

int main() {
  int n;
  std::cin >> n;

  std::vector<int> a(n);
  for (int i = 0; i < n; i++)
    std::cin >> a[i];

  int m;
  std::cin >> m;
  int *begin = a.data();
  for (int i = 0; i < m; i++) {
    int x;
    std::cin >> x;

    int *res = begin;

    if (a[0] == x ||
        ((res = BinarySearch(begin, begin + n, [&](int X) -> bool {
            return X >= x;
          }) + 1) - begin < n && *res == x)) {
      std::cout << res - begin + 1 << " ";

      if (a[n - 1] == x) {
        res = &a[n - 1];
      } else {
        res = BinarySearch(res, begin + n, [&](int X) -> bool {
            return X > x;
          });
      }
      std::cout << res - begin + 1 << std::endl;
    }
    else
      std::cout << "-1 -1" << std::endl;
  }
  return 0;
}
