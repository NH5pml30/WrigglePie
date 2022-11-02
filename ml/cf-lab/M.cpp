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

  std::vector<int64_t> xs;
  xs.reserve(noof_objs);
  std::vector<std::vector<int64_t>> y2xs(noof_classes);

  for (int i = 0; i < noof_objs; i++)
  {
    int64_t x, y;
    std::cin >> x >> y;
    xs.push_back(x);
    y2xs[y - 1].push_back(x);
  }

  auto count_in_sum = [&](std::ranges::random_access_range auto &&r) {
    int n = (int)std::ranges::size(r);
    std::ranges::sort(r);
    std::vector<int64_t> sums(n + 1);
    std::partial_sum(r.begin(), r.end(), ++sums.begin(), std::plus<int64_t>{});

    int64_t all_sum = 0;
    for (int i = 0; i < n; i++)
    {
      all_sum += int64_t{r[i]} * i - sums[i];
      all_sum += sums.back() - sums[i + 1] - int64_t{r[i]} * (n - i - 1);
    }

    return all_sum;
  };

  int64_t in_sum = 0;
  for (int c = 0; c < noof_classes; c++)
    in_sum += count_in_sum(y2xs[c]);
  std::cout << in_sum << '\n' << count_in_sum(xs) - in_sum << std::endl;
  return 0;
}
