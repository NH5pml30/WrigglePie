#include <iostream>
#include <vector>
#include <tuple>
#include <algorithm>
#include <numeric>
#include <functional>
#include <iomanip>
#include <cmath>

template<typename T>
using statistic_t = std::tuple<T, T, T>;

using statistici_t = std::tuple<int, int, int>;

template<typename T>
statistic_t<T> operator+(statistic_t<T> lhs, statistic_t<T> rhs) {
  auto [tpl, fpl, fnl] = lhs;
  auto [tpr, fpr, fnr] = rhs;
  return statistic_t<T>{tpl + tpr, fpl + fpr, fnl + fnr};
}

template<typename T>
statistic_t<double> operator*(statistic_t<T> lhs, double rhs)
{
  auto [tpl, fpl, fnl] = lhs;
  return statistic_t<double>{tpl * rhs, fpl * rhs, fnl * rhs};
}

statistici_t count_tp_fp_fn(const std::vector<std::vector<int>> &ct, int i)
{
  int tp = ct[i][i];
  int fn = std::reduce(ct[i].begin(), ct[i].end(), 0) - tp;
  int fp = std::transform_reduce(ct.begin(), ct.end(), 0, std::plus<>{},
                                 [i](auto &cti) { return cti[i]; }) - tp;
  return {tp, fp, fn};
}

template<typename T>
double count_f1_score(statistic_t<T> stat)
{
  auto [tp, fp, fn] = stat;
  if (2 * tp + fp + fn == 0)
    return 0;
  return 2 * tp * 1.0 / (2 * tp + fp + fn);
}

double count_f1_score(double prec, double recall)
{
  if ((prec == 0 && recall == 0) || (std::isnan(prec) || std::isnan(recall)))
    return 0;
  return 2 * prec * recall / (prec + recall);
}

template<typename T>
double count_precision(statistic_t<T> stat)
{
  auto [tp, fp, fn] = stat;
  if (tp + fp == 0)
    return 0;
  return tp * 1.0 / (tp + fp);
}

template<typename T>
double count_recall(statistic_t<T> stat)
{
  auto [tp, fp, fn] = stat;
  if (tp + fn == 0)
    return 0;
  return tp * 1.0 / (tp + fn);
}

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int k;
  std::cin >> k;

  std::vector<std::vector<int>> ct(k);
  for (auto &row : ct)
  {
    row.resize(k);
    for (auto &el : row)
      std::cin >> el;
  }

  std::vector<statistici_t> stats(k);
  for (int i = 0; i < k; i++)
    stats[i] = count_tp_fp_fn(ct, i);

  std::vector<int> counts(k);
  for (int i = 0; i < k; i++)
    counts[i] = std::reduce(ct[i].begin(), ct[i].end(), 0);
  int all_count = std::reduce(counts.begin(), counts.end(), 0);

  // micro
  statistic_t<double> all{0, 0, 0};
  for (int i = 0; i < k; i++)
    all = all + stats[i] * (counts[i] * 1.0 / all_count);
  std::cout << std::setprecision(9) << count_f1_score(all) << std::endl;

  auto weighted_map = [&](auto &&fun, auto init) {
    for (int i = 0; i < k; i++)
      if (counts[i] > 0)
        init += fun(stats[i]) * counts[i] / all_count;
    return init;
  };

  // macro
  double prec = weighted_map(count_precision<int>, 0.0);
  double recall = weighted_map(count_recall<int>, 0.0);
  std::cout << std::setprecision(9) << count_f1_score(prec, recall) << std::endl;

  // avg
  double avg = weighted_map(count_f1_score<int>, 0.0);
  std::cout << std::setprecision(9) << avg << std::endl;

  return 0;
}
