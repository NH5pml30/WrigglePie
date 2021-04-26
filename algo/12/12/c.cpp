/* Nikolai Kholiavin, M3238 */

#include <iostream>
#include <string_view>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>

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
  std::string s;
  std::cin >> s;
  std::vector<int> pi = prefix_func(s);
  std::for_each(pi.begin() + 1, pi.end(), [&](int num) { std::cout << num << ' '; });
  std::cout << std::endl;
  return 0;
}
