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
#include <bitset>
#include <map>

template<class It, class Callback, class Accum, class AccumAppend>
void gen_subsets_helper(It begin, It end, Callback &&callback, AccumAppend &&append,
                        Accum accum = {}, int mask = 0, int pos = 0) {
  if (begin == end) {
    callback(mask, accum);
    return;
  }

  gen_subsets_helper(std::next(begin), end, std::forward<Callback>(callback),
                     std::forward<AccumAppend>(append), append(accum, *begin, false), mask,
                     pos + 1);
  gen_subsets_helper(std::next(begin), end, std::forward<Callback>(callback),
                     std::forward<AccumAppend>(append), append(accum, *begin, true),
                     mask | (1 << pos), pos + 1);
}

template<class It, class Callback, class Accum, class AccumAppend>
void gen_subsets(It begin, It end, Callback &&callback, AccumAppend &&append, Accum accum = {}) {
  gen_subsets_helper(begin, end, std::forward<Callback>(callback), std::forward<AccumAppend>(append), std::move(accum));
}

template<class It, class Callback, class Accum, class AccumAppend, class T = decltype(*std::declval<It>())>
void gen_combinations_helper(It begin, It end, int k, Callback &&callback, AccumAppend &&append,
                             std::vector<T> &p_val, std::vector<int> &p, int n, Accum accum = {}) {
  if ((int)p.size() == k) {
    callback(p, p_val, accum);
    return;
  }

  int i = 0;
  for (It iter = begin; iter != end; i++, ++iter) {
    if ((p.empty() || i > p.back()) && n - i - 1 >= k - (int)p.size() - 1) {
      p.push_back(i);
      p_val.push_back(*iter);
      gen_combinations_helper(begin, end, k, std::forward<Callback>(callback),
                              std::forward<AccumAppend>(append), p_val, p, n, append(accum, i, *iter));
      p.pop_back();
      p_val.pop_back();
    }
  }
}

template<class It, class Callback, class Accum, class AccumAppend>
void gen_combinations(It begin, It end, int k, Callback &&callback, AccumAppend &&append,
                      Accum accum = {}) {
  std::vector<std::remove_reference_t<decltype(*begin)>> p_val;
  std::vector<int> p;
  gen_combinations_helper(begin, end, k, std::forward<Callback>(callback),
                          std::forward<AccumAppend>(append), p_val, p,
                          (int)std::distance(begin, end), std::move(accum));
}

template<class It, class Callback, class Accum, class AccumAppend, class T = decltype(*std::declval<It>())>
void gen_permutations_helper(It begin, It end, Callback &&callback, AccumAppend &&append,
                             std::vector<T> &p_val, std::vector<int> &p, std::vector<bool> &was, Accum accum = {}) {
  if (p.size() == was.size()) {
    callback(p, p_val, accum);
    return;
  }

  int i = 0;
  for (It iter = begin; iter != end; i++, ++iter) {
    if (!was[i]) {
      was[i] = true;
      p.push_back(i);
      p_val.push_back(*iter);
      gen_permutations_helper(begin, end, std::forward<Callback>(callback),
                              std::forward<AccumAppend>(append), p_val, p, was, append(accum, i, *iter));
      was[i] = false;
      p_val.pop_back();
      p.pop_back();
    }
  }
}

template<class It, class Callback, class Accum, class AccumAppend>
void gen_permutations(It begin, It end, Callback &&callback, AccumAppend &&append,
                      Accum accum = {}) {
  std::vector<bool> was(std::distance(begin, end));
  std::vector<int> p;
  std::vector<std::remove_reference_t<decltype(*begin)>> p_val;
  gen_permutations_helper(begin, end, std::forward<Callback>(callback),
                          std::forward<AccumAppend>(append), p_val, p, was, std::move(accum));
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto &&in = std::cin;

  int n, m;
  in >> n >> m;

  std::set<int> missing_s;
  {
    std::vector<int> all_v(n);
    std::iota(all_v.begin(), all_v.end(), 0);
    missing_s = std::set<int>(all_v.begin(), all_v.end());
  }

  std::vector<int> a(n), missing_pos;
  for (int i = 0; i < n; i++) {
    in >> a[i];
    --a[i];
    if (a[i] != -1)
      missing_s.erase(a[i]);
    else
      missing_pos.push_back(i);
  }

  std::vector<int> missing(missing_s.begin(), missing_s.end());

  int n_missing = (int)missing.size();

  auto build_sortedness_indices = [](auto begin, auto end, auto src_begin, auto src_end,
                                     auto &&comparator) {
    std::vector<std::vector<std::remove_reference_t<decltype(*begin)>>> res(
        std::distance(begin, end));
    int i = 0;
    for (auto it = begin; it != end; ++it, i++) {
      int j = 0;
      for (auto src_it = src_begin; src_it != src_end; ++src_it, j++) {
        int last = i == 0 ? 0 : res[i - 1][j];
        res[i].push_back(last + (*it != -1 && comparator(*it, *src_it)));
      }
    }
    return res;
  };

  std::vector<std::vector<int>> n_less_on_prefix;
  std::vector<std::vector<int>> n_greater_on_suffix;
  {
    std::vector<std::vector<int>> n_less_on_prefix_ =
        build_sortedness_indices(a.begin(), a.end(), missing.begin(), missing.end(), std::less<>());
    std::vector<std::vector<int>> n_greater_on_suffix_ = build_sortedness_indices(
        a.rbegin(), a.rend(), missing.begin(), missing.end(), std::greater<>());
    std::reverse(n_greater_on_suffix_.begin(), n_greater_on_suffix_.end());

    for (auto pos : missing_pos)
      n_less_on_prefix.emplace_back(std::move(n_less_on_prefix_[pos]));
    for (auto pos : missing_pos)
      n_greater_on_suffix.emplace_back(std::move(n_greater_on_suffix_[pos]));
  }

  for (auto iter = a.begin(); iter != a.end(); ++iter)
    if (*iter != -1)
      m -= (int)std::count_if(a.begin(), iter, [val=*iter](int x) { return x != -1 && x < val; });

  uint64_t ans = 0;

  gen_combinations(
      missing.begin(), missing.end(), (int)missing.size() / 2,
      [&](auto &left_set_i, auto &left_set_v, int mask) {
        std::vector<int> right_set_v, right_set_i;
        for (int i = 0; i < n_missing; i++)
          if (((mask >> i) & 1) == 0) {
            right_set_v.push_back(missing[i]);
            right_set_i.push_back(i);
          }

        int left_right_sortedness = 0;
        for (auto el : right_set_i)
          left_right_sortedness +=
              (int)std::count_if(left_set_i.begin(), left_set_i.end(), [el](int x) { return x < el; });

        std::map<int, int> left_sortedness;

        gen_permutations(
            left_set_i.begin(), left_set_i.end(), [&](auto &, auto &left_i_v, int) {
              int sortedness = 0, i = 0;
              for (auto ind : left_i_v) {
                sortedness +=
                    n_less_on_prefix[i][ind] + n_greater_on_suffix[i][ind] +
                    (int)std::count_if(left_i_v.begin(), left_i_v.begin() + i,
                                       [ind](int x) { return x < ind; });
                i++;
              }
              left_sortedness[sortedness]++;
            },
            [](int x, ...) { return x; }, 0);

        gen_permutations(
            right_set_i.begin(), right_set_i.end(), [&](auto &, auto &right_i_v, int) {
              int sortedness = 0, i = 0;
              for (auto ind : right_i_v) {
                sortedness +=
                    n_less_on_prefix[(int)left_set_v.size() + i][ind] +
                    n_greater_on_suffix[(int)left_set_v.size() + i][ind] +
                    (int)std::count_if(right_i_v.begin(), right_i_v.begin() + i,
                                       [ind](int x) { return x < ind; });
                i++;
              }
              if (auto iter = left_sortedness.find(m - sortedness - left_right_sortedness); iter != left_sortedness.end())
                ans += iter->second;
            },
            [](int x, ...) { return x; }, 0);
      },
      [](int accum, int i, int) { return accum | (1 << i); }, 0);

  std::cout << ans << std::endl;
  return 0;
}
