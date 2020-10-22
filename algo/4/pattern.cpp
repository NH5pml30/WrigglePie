/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

int main() {
  std::string a, s;

  std::getline(std::cin, s);
  std::getline(std::cin, a);

  if (a.size() == 0) {
    for (auto ch : s)
      if (ch != '*') {
        std::cout << "NO" << std::endl;
        return 0;
      }
    std::cout << "YES" << std::endl;
    return 0;
  }
  if (s.size() == 0) {
    std::cout << "NO" << std::endl;
    return 0;
  }

  std::vector<std::vector<bool>> f(a.size() + 1);
  auto get = [&](int i, int j) -> bool {
    if (j == -1)
      return i == 0;
    return f[i][j];
  };

  f[0].resize(s.size());
  for (size_t j = 0; j < s.size(); j++) {
    if (s[j] == '*')
      f[0][j] = get(0, j - 1);
    else
      f[0][j] = false;
  }
  for (size_t i = 1; i <= a.size(); i++) {
    f[i].resize(s.size());
    for (size_t j = 0; j < s.size(); j++) {
      if (s[j] == '?') {
        f[i][j] = get(i - 1, j - 1);
      } else if (s[j] == '*') {
        f[i][j] = get(i, j - 1) || get(i - 1, j);
      } else {
        if (i > 0 && s[j] == a[i - 1])
          f[i][j] = get(i - 1, j - 1);
        else
          f[i][j] = false;
      }
    }
  }

  std::cout << (f[a.size()][s.size() - 1] ? "YES" : "NO") << std::endl;

  return 0;
}
