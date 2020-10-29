/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>

int main() {
  int n;
  double A;
  std::cin >> n >> A;

  double
    x = (n - 2.0) / (n - 1) * A - n + 2,
    max_b = std::numeric_limits<double>::lowest();

  for (int i = 0; i < n; i++)
    if (double hi = i * x - (i - 1) * (A - i),
        b = -hi * (n - 1) / i; b > max_b)
      max_b = b;

  std::cout << std::fixed << std::setprecision(2) << max_b;
  return 0;
}


