/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <cmath>
#include <algorithm>

int main()
{
  double eps;
  std::cin >> eps;

  // Hoeffding's inequality =>
  int n = static_cast<int>(std::log(200) / (2 * eps * eps));
  double sum = 0.0;
  for (int i = 0; i < n; i++)
  {
    std::cout << -1 << std::endl;
    int res;
    std::cin >> res;
    sum += res;
  }
  std::cout << sum / std::max(n, 1) << std::endl;
  return 0;
}
