/* Nikolai Kholiavin, M3138 */
#include <fstream>
#include <vector>
#include <algorithm>
#include <string>

int main() {
  std::string name = "levenshtein";
  std::ifstream in(name + ".in");
  std::ofstream out(name + ".out");

  std::string a, b;
  in >> a >> b;

  std::vector<std::vector<int>> f(a.size());
  auto get = [&](int i, int j) {
    if (i < 0)
      return std::max(j + 1, 0);
    if (j < 0)
      return std::max(i + 1, 0);
    return f[i][j];
  };

  for (size_t i = 0; i < a.size(); i++) {
    f[i].resize(b.size());
    for (size_t j = 0; j < b.size(); j++) {
      f[i][j] = std::min(get(i - 1, j) + 1,
                         std::min(get(i, j - 1) + 1,
                                  get(i - 1, j - 1) + (a[i] != b[j])));
    }
  }

  out << f[a.size() - 1][b.size() - 1] << std::endl;

  return 0;
}
