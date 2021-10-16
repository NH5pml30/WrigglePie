/* Nikolai Kholiavin, M3238 */
#include <vector>
#include <random>
#include <iostream>
#include <set>
#include <algorithm>
#include <functional>
#include <numeric>
#include <cassert>

std::uniform_real_distribution<> normalized_random_generator(0, 1);
std::default_random_engine engine{};

struct discrete_int_distribution
{
  std::vector<int> donors;
  std::vector<double> thresholds;

  discrete_int_distribution(const std::vector<int> &probs) : donors(probs.size()), thresholds(probs.size(), 0.0)
  {
    std::iota(donors.begin(), donors.end(), 0);
    int n = (int)probs.size();

    int sum = std::reduce(probs.begin(), probs.end(), 0);
    using pr_ind_t = std::pair<int, int>;
    std::vector<pr_ind_t> below_avg, above_avg;

    auto sort_pair = [&](pr_ind_t pair) {
      if (pair.first > sum)
        above_avg.push_back(pair);
      else if (pair.first < sum)
        below_avg.push_back(pair);
    };

    for (int i = 0; i < n; i++)
      sort_pair({probs[i] * n, i});

    while (!below_avg.empty())
    {
      auto [min, min_i] = below_avg.back();
      auto [max, max_i] = above_avg.back();
      below_avg.pop_back(), above_avg.pop_back();

      max -= sum - min;
      donors[min_i] = max_i;
      thresholds[min_i] = min * 1.0 / sum;
      if (max > 0)
        sort_pair({max, max_i});
    }
    assert(above_avg.empty());
  }

  template<class Generator>
  int operator()(Generator &g)
  {
    int i = static_cast<int>(normalized_random_generator(g) * donors.size());
    return normalized_random_generator(g) < thresholds[i] ? i : donors[i];
  }
};

int main()
{
  int n, q;
  std::cin >> n >> q;

  std::vector<int> f(n);
  for (int i = 0; i < n; i++)
    std::cin >> f[i];

  discrete_int_distribution distr(f);
  std::vector<int> g(n);

  for (int i = 0; i < q; i++)
    std::cout << distr(engine) + 1 << ' ';
  std::cout << std::endl;
  return 0;
}
