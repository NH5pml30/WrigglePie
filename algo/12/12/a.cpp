/* Nikolai Kholiavin, M3238 */

#include <iostream>
#include <string_view>
#include <string>
#include <vector>

int main() {
  std::string p, t;
  std::cin >> p >> t;
  std::string_view pv = p, tv = t;
  std::vector<int> res;
  for (int pos = 0; tv.length() >= pv.length(); tv.remove_prefix(1), ++pos)
    if (tv.substr(0, pv.length()) == pv)
      res.push_back(pos);
  std::cout << res.size() << '\n';
  for (auto &el : res)
    std::cout << el + 1 << ' ';
  std::cout << std::endl;
  return 0;
}
