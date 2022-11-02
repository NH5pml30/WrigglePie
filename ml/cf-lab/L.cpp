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

  int N;
  std::cin >> N;
  std::vector<std::pair<double, double>> vals(N);
  for (auto &[x, y] : vals)
    std::cin >> x >> y;

  auto get_ranks = [](std::ranges::random_access_range auto &&r) {
    int N = std::ranges::size(r);
    std::vector<int> inds = materialize(std::views::iota(0, N));
    std::ranges::sort(inds, {}, [&r](int i) { return r[i]; });

    std::vector<int> ranks(N);
    for (int i = 0; i < N; i++)
      ranks[inds[i]] = i + 1;
    return ranks;
  };

  auto ranks_x = get_ranks(vals | std::views::elements<0>);
  auto ranks_y = get_ranks(vals | std::views::elements<1>);

  std::cout << 1 - 6 / ((N * (N - 1.0) * (N + 1.0))) *
                       inner_product(ranks_x, ranks_y, 0.0, std::plus<>{},
                                     [](double l, double r) { return sqr(l - r); });
  return 0;
}
