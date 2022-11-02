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

  int noof_classes, noof_objs;
  std::cin >> noof_classes >> noof_objs;

  std::vector<std::vector<int>> x2ys(noof_classes);

  for (int i = 0; i < noof_objs; i++)
  {
    int x, y;
    std::cin >> x >> y;
    x2ys[x - 1].push_back(y);
  }

  auto mean = [](std::ranges::range auto &&r) requires(requires { std::ranges::size(r); })
  {
    long double mean = fold(
        r | std::views::transform([s = std::ranges::size(r)](long double v) { return v / s; }));
    return r | std::views::transform([mean](long double x) { return x - mean; });
  };

  long double sum = 0;
  for (auto &ys : x2ys)
    sum += fold(mean(ys) | std::views::transform([noof_objs](long double val) {
                  return sqr(val) / noof_objs;
                }));

  std::cout << std::setprecision(10) << sum << std::endl;
  return 0;
}
