/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>

int main() {
  int n;
  std::cin >> n;
  std::vector<int> data(n);
  for (int i = 0; i < n; i++) {
    std::cin >> data[i];
  }

  for (int i = 1; 2 * i <= n; i++) {
    if (data[i - 1] > data[2 * i - 1] ||
        (2 * i + 1 <= n && data[i - 1] > data[2 * i])) {
      std::cout << "NO" << std::endl;
      return 0;
    }
  }

  std::cout << "YES" << std::endl;
  return 0;
}
