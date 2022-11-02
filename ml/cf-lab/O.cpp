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

auto fold(std::ranges::range auto &&range, auto init)
{
  return std::reduce(std::begin(range), std::end(range), std::move(init));
}

template<typename T, typename ReduceOp = std::plus<>, typename BinOp = std::multiplies<>>
auto inner_product(std::ranges::range auto &&r1, std::ranges::range auto &&r2, T init = T{},
                   ReduceOp rop = {}, BinOp bop = {})
{
  return std::inner_product(r1.begin(), r1.end(), r2.begin(), init, std::move(rop), std::move(bop));
}

template<template<typename> typename T = std::vector>
auto materialize(std::ranges::range auto &&range)
{
  auto r = range | std::views::common;
  return T<std::ranges::range_value_t<decltype(r)>>(std::begin(r), std::end(r));
}

auto sqr(auto x)
{
  return x * x;
}

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int k1, k2;
  std::cin >> k1 >> k2;

  std::vector<int> row_sums(k1);
  std::vector<int> col_sums(k2);

  int N;
  std::cin >> N;
  std::map<int64_t, int> cnts;
  for (int i = 0; i < N; i++)
  {
    int x, y;
    std::cin >> x >> y;
    row_sums[x - 1]++;
    col_sums[y - 1]++;
    cnts[int64_t{x - 1} * k2 + y - 1]++;
  }

  double res = 1;
  for (auto [packed, cnt] : cnts)
  {
    int x = packed / k2, y = packed % k2;
    double p = row_sums[x] * 1.0 / N * col_sums[y] / N;
    res += -p + sqr(cnt * 1.0 / N - p) / p;
  }

  std::cout << std::setprecision(10) << N * res << std::endl;
  return 0;
}
