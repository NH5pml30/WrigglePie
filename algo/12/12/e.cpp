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
  std::vector<int> z(text.size());
  z[0] = 0;
  for (size_t pos = 1; pos < text.length(); ++pos) {
    z[pos] = 0;
    if (pos < R)
      z[pos] = std::min(z[pos - L], (int)(R - pos));
    while (pos + z[pos] < text.length() && text[z[pos]] == text[pos + z[pos]])
      ++z[pos];
    if (pos + z[pos] > R) {
      L = pos;
      R = pos + z[pos];
    }
  }
  return z;
}

int main() {
  auto reverse_string = [](std::string_view sv) {
    std::string res{sv};
    std::reverse(res.begin(), res.end());
    return res;
  };

  std::string p, t;
  std::cin >> p >> t;

  std::string s = p;
  s.append("#").append(t);

  std::string s_rev = reverse_string(p);
  s_rev.append("#").append(reverse_string(t));

  std::vector<int> z = z_func(s), z_rev = z_func(s_rev);

  auto lcp = [&](const std::vector<int> &z, int offset) { return z[p.size() + 1 + offset]; };

  std::vector<int> res;

  for (int i = 0; i < (int)(t.size() - p.size() + 1); i++) {
    if (lcp(z, i) + lcp(z_rev, t.size() - i - p.size()) >= (int)p.size() - 1)
      res.push_back(i + 1);
  }

  std::cout << res.size() << '\n';
  std::for_each(res.begin(), res.end(), [&](auto &&v) { std::cout << v << ' '; });
  std::cout << std::endl;
  return 0;
}
