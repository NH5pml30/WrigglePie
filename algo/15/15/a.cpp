/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <cassert>
#include <stack>
#include <optional>
#include <variant>
#include <sstream>
#include <random>

template<class It, class Callback>
void gen_subsets_helper(It begin, It end, Callback &&callback, int mask = 0, int pos = 0, uint64_t weight = 0) {
  if (begin == end) {
    callback(mask, weight);
    return;
  }

  gen_subsets_helper(std::next(begin), end, std::forward<Callback>(callback), mask, pos + 1, weight);
  gen_subsets_helper(std::next(begin), end, std::forward<Callback>(callback), mask | (1 << pos), pos + 1, weight + *begin);
}

template<class It, class Callback>
void gen_subsets(It begin, It end, Callback &&callback) {
  gen_subsets_helper(begin, end, std::forward<Callback>(callback));
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  // #ifdef _DEBUG
  //   auto &&in = std::istringstream(R"_delim(1
  // 1
  // 1)_delim");
  // #else
  auto &&in = std::cin;
  // #endif

  int n;
  in >> n;
  std::vector<unsigned> x(n);
  for (int i = 0; i < n; i++)
    in >> x[i];
  unsigned C;
  in >> C;

  unsigned ans = 0;

  std::vector<int> left_subsets_weight;
  gen_subsets(x.begin(), x.begin() + n / 2, [&](int, uint64_t weight) {
    if (weight <= uint64_t{C})
      left_subsets_weight.push_back((int)weight);
  });
  std::sort(left_subsets_weight.begin(), left_subsets_weight.end());

  gen_subsets(x.begin() + n / 2, x.end(), [&](int, uint64_t weight) {
    if (weight <= uint64_t{C})
      ans += (unsigned)(std::upper_bound(left_subsets_weight.begin(), left_subsets_weight.end(),
                                         (int)(C - weight)) -
                        left_subsets_weight.begin());
  });

  std::cout << ans << std::endl;
  return 0;
}
