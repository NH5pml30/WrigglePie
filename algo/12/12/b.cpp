/* Nikolai Kholiavin, M3238 */

#include <iostream>
#include <string_view>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>

std::vector<int> prefix_func(std::string_view text) {
  std::vector<int> pi(text.size() + 1);
  int k = pi[0] = pi[1] = 0;
  for (size_t pos = 1; pos < text.length(); ++pos) {
    while (k > 0 && text[k] != text[pos])
      k = pi[k];
    if (text[k] == text[pos])
      ++k;
    pi[pos + 1] = k;
  }
  return pi;
}

int main() {
  std::string p, t;
  std::cin >> p >> t;
  std::vector<int> pi = prefix_func(p + "#" + t), res;
  auto check_and_save = [&, i = 0](int pri) mutable {
    ++i;
    if (pri == (int)p.length()) {
      res.push_back(i - 2 * p.length() - 1);
      return true;
    }
    return false;
  };
  std::cout << std::count_if(pi.begin(), pi.end(), check_and_save) << '\n';
  for (auto &el : res)
    std::cout << el << ' ';
  std::cout << std::endl;
  return 0;
}
