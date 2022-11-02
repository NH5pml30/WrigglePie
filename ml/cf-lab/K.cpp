#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <numbers>
#include <numeric>
#include <random>
#include <ranges>
#include <set>
#include <span>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

auto fold(std::ranges::range auto &&range)
{
  return std::reduce(std::begin(range), std::end(range));
}

template<typename T>
auto inner_product(std::ranges::range auto &&r1, std::ranges::range auto &&r2, T init = T{})
{
  return std::inner_product(r1.begin(), r1.end(), r2.begin(), init);
}

auto sqr(auto x)
{
  return x * x;
}

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int N;
  std::cin >> N;
  std::vector<std::pair<double, double>> vals(N);
  for (auto &[x, y] : vals)
    std::cin >> x >> y;

  auto x = vals | std::views::elements<0>;
  auto y = vals | std::views::elements<1>;

  auto mean = [](std::ranges::range auto &&r) requires(requires { std::ranges::size(r); })
  {
    double mean = fold(r | std::views::transform([s = std::ranges::size(r)](double v) { return v / s; }));
    return r | std::views::transform([mean](double x) { return x - mean; });
  };

  auto x_z = mean(x);
  auto y_z = mean(y);

  std::cout << inner_product(x_z, y_z, 0.0) / sqrt(fold(x_z | std::views::transform(sqr<double>)) *
                                                   fold(y_z | std::views::transform(sqr<double>)))
            << std::endl;
  return 0;
}
