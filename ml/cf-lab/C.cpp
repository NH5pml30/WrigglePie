#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <tuple>
#include <vector>
#include <string>
#include <numbers>
#include <variant>

using obj_t = std::pair<std::vector<int>, int>;

double sqr(double val)
{
  return val * val;
}

double cube(double val)
{
  return sqr(val) * val;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n, m;
  std::cin >> n >> m;

  std::vector<obj_t> objs(n);
  for (auto &obj : objs)
  {
    obj.first.resize(m);
    for (auto &feat : obj.first)
      std::cin >> feat;
    std::cin >> obj.second;
  }

  obj_t query;
  query.first.resize(m);
  for (auto &feat : query.first)
    std::cin >> feat;

  std::string metric;
  std::cin >> metric;

  std::function<double(const obj_t &, const obj_t &)> metric_fun;

  if (metric == "manhattan")
    metric_fun = [](const obj_t &lhs, const obj_t &rhs) -> double {
      return std::transform_reduce(lhs.first.begin(), lhs.first.end(), rhs.first.begin(), 0,
                                   std::plus<>{}, [](int l, int r) { return std::abs(l - r); });
    };
  else if (metric == "euclidean")
    metric_fun = [](const obj_t &lhs, const obj_t &rhs) -> double {
      return sqrt(std::transform_reduce(lhs.first.begin(), lhs.first.end(), rhs.first.begin(), 0.0,
                                        std::plus<>{}, [](int l, int r) { return sqr(l - r); }));
    };
  else if (metric == "chebyshev")
    metric_fun = [](const obj_t &lhs, const obj_t &rhs) -> double {
      return std::transform_reduce(lhs.first.begin(), lhs.first.end(), rhs.first.begin(), 0,
                                   (const int &(*)(const int &, const int &))std::max,
                                   [](int l, int r) { return std::abs(l - r); });
    };

  auto finite = [](auto &&kernel) {
    return [kernel](double val) { return std::abs(val) > 1 ? 0 : kernel(val); };
  };

  std::string kernel;
  std::cin >> kernel;

  std::function<double(double)> kernel_fun;

  if (kernel == "uniform")
    kernel_fun = finite([](double u) { return std::abs(u) == 1 ? 0 : 0.5; });
  else if (kernel == "triangular")
    kernel_fun = finite([](double u) { return 1 - std::abs(u); });
  else if (kernel == "epanechnikov")
    kernel_fun = finite([](double u) { return 0.75 * (1 - sqr(u)); });
  else if (kernel == "quartic")
    kernel_fun = finite([](double u) { return 15.0 / 16 * sqr(1 - sqr(u)); });
  else if (kernel == "triweight")
    kernel_fun = finite([](double u) { return 35.0 / 32 * cube(1 - sqr(u)); });
  else if (kernel == "tricube")
    kernel_fun = finite([](double u) { return 70.0 / 81 * cube(1 - cube(std::abs(u))); });
  else if (kernel == "gaussian")
    kernel_fun = [](double u) { return 1 / sqrt(2 * std::numbers::pi) * std::exp(-0.5 * sqr(u)); };
  else if (kernel == "cosine")
    kernel_fun =
        finite([](double u) { return std::numbers::pi / 4 * std::cos(std::numbers::pi / 2 * u); });
  else if (kernel == "logistic")
    kernel_fun = [](double u) { return 1 / (std::exp(u) + 2 + std::exp(-u)); };
  else if (kernel == "sigmoid")
    kernel_fun = [](double u) { return 2 / std::numbers::pi / (std::exp(u) + std::exp(-u)); };

  std::string wt;
  int windowParam;
  std::cin >> wt >> windowParam;
  double windowSize;
  if (wt == "fixed")
  {
    windowSize = windowParam;
  }
  else if (wt == "variable")
  {
    std::vector<double> obj_dists;
    obj_dists.reserve(n);
    std::transform(objs.begin(), objs.end(), std::back_inserter(obj_dists),
                   [&metric_fun, &query](const auto &obj) { return metric_fun(query, obj); });
    std::sort(obj_dists.begin(), obj_dists.end());

    windowSize = obj_dists[windowParam];
  }

  std::vector<double> smooth;
  smooth.reserve(n);
  std::transform(objs.begin(), objs.end(), std::back_inserter(smooth),
                 [&kernel_fun, &wt, &metric_fun, &query, windowSize](const obj_t &obj) {
                   auto dist = metric_fun(query, obj);
                   if (windowSize == 0 && dist == 0)
                     return 1.0;
                   return kernel_fun(dist / windowSize);
                 });

  double val = std::transform_reduce(objs.begin(), objs.end(), smooth.begin(), 0.0, std::plus<>{},
                                     [&kernel_fun, &metric_fun, &query, windowSize](
                                         const obj_t &obj, double k) { return obj.second * k; }) /
               std::reduce(smooth.begin(), smooth.end());

  if (std::isinf(val) || std::isnan(val))
    val = std::transform_reduce(objs.begin(), objs.end(), 0.0, std::plus<>{},
                                [](const obj_t &obj) { return obj.second; }) *
          1.0 / n;

  std::cout << std::setprecision(9) << val << std::endl;
  return 0;
}
