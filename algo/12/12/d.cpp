/* Nikolai Kholiavin, M3238 */

#include <iostream>
#include <string_view>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>

std::vector<int> z_func(std::string_view text) {
  size_t L = 0, R = 0;
  std::vector<int> z(text.size() + 1);
  z[0] = z[1] = 0;
  for (size_t pos = 1; pos < text.length(); ++pos) {
    z[pos + 1] = 0;
    if (pos < R)
      z[pos + 1] = std::min(z[pos - L + 1], (int)(R - pos));
    while (pos + z[pos + 1] < text.length() && text[z[pos + 1]] == text[pos + z[pos + 1]])
      ++z[pos + 1];
    if (pos + z[pos + 1] > R) {
      L = pos;
      R = pos + z[pos + 1];
    }
  }
  return z;
}

int main() {
  std::string s;
  std::cin >> s;
  std::vector<int> z = z_func(s);
  std::for_each(z.begin() + 2, z.end(), [&](int num) { std::cout << num << ' '; });
  std::cout << std::endl;
  return 0;
}
