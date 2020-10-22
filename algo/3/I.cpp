/* Nikolai Kholiavin, M3138 */
#include <iostream>

int main() {
  int n, m;
  std::cin >> n >> m;

  if (m == 1) {
    std::cout << "0 1 1" << std::endl;
  } else if (m == 2) {
    std::cout << "1 2 1" << std::endl <<
                 "0 1 1" << std::endl;
  } else {
    int n = 0, cur = 0;
    for (int i = 0; i < m; i++) {
      if (cur >= n / 2) {
        std::cout << "1 " << n + 2 << ' ' << n + 1 << std::endl;
        n++;
        cur = 0;
      } else {
        std::cout << "0 " << cur + 1 << ' ' << n - cur - 1 << std::endl;
        cur++;
      }
    }
  }
  return 0;
}
