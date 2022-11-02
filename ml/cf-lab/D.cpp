#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
#include <numbers>
#include <numeric>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

int sign(int64_t val)
{
  if (val > 0)
    return 1;
  else if (val < 0)
    return -1;
  return 0;
}

int64_t sqr(int64_t val)
{
  return val * val;
}

int main() {
  int n, m;
  std::cin >> n >> m;

  std::vector<std::vector<int64_t>> x(n);
  std::vector<int64_t> y(n);
  for (int i = 0; i < n; i++)
  {
    x[i].reserve(m + 1);
    x[i].resize(m);
    for (auto &el : x[i])
      std::cin >> el;
    x[i].push_back(1);
    std::cin >> y[i];
  }

  std::vector<int64_t> a(m + 1);
  for (auto &el : a)
    std::cin >> el;

  std::vector<int64_t> y_hat;
  y_hat.reserve(n);
  for (auto &xi : x)
    y_hat.push_back(std::transform_reduce(xi.begin(), xi.end(), a.begin(), int64_t{0}));

  for (int i = 0; i < n; i++, std::cout << '\n')
    for (int j = 0; j < m + 1; j++, std::cout << ' ')
      std::cout << std::setprecision(9)
                << sign(y_hat[i] - y[i]) *
                       (std::abs(y[i]) + std::abs(y_hat[i]) + sign(y_hat[i]) * (y[i] - y_hat[i])) *
                       1.0 / sqr(std::abs(y[i]) + std::abs(y_hat[i])) * x[i][j];
  std::cout.flush();
  return 0;
}
