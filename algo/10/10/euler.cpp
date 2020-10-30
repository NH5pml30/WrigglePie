/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <optional>
#include <numeric>
#include <algorithm>
#include <functional>
#include <cassert>
#include <unordered_set>
#include <unordered_map>
#include <iterator>
#include <set>

template<class EdgeCallback>
void euler_(std::vector<std::vector<int>> &mat, EdgeCallback &callback, int v) {
  for (int u = 0; u < (int)mat.size(); u++)
    if (mat[v][u]) {
      int deg = mat[v][u];
      mat[v][u] = mat[u][v] = 1 - deg % 2;
      euler_(mat, callback, u);
      while (deg > 2) {
        callback(v, u);
        callback(u, v);
        deg -= 2;
      }
      callback(v, u);
    }
}

template<class EdgeCallback>
void euler(std::vector<std::vector<int>> &&mat, EdgeCallback callback) {
  int start = 0;
  for (int v = 0; v < (int)mat.size(); v++) {
    int deg = 0;
    for (int u = 0; u < (int)mat.size(); u++)
      deg += mat[v][u];
    if (deg % 2 == 1) {
      start = v;
      break;
    }
  }

  euler_(mat, callback, start);
}

std::pair<std::vector<std::vector<int>>, int> read_graph(std::istream &in) {
  int n;
  in >> n;

  int m = 0;
  std::vector<std::vector<int>> res(n);
  for (int i = 0; i < n; i++) {
    res[i].resize(n);
    int mi;
    std::cin >> mi;
    for (int j = 0; j < mi; j++) {
      int other;
      std::cin >> other;
      res[i][other - 1]++;
      m++;
    }
  }

  return {res, m / 2};
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto[g, m] = read_graph(std::cin);
  std::cout << m << std::endl;
  euler(std::move(g), [&, is_first = true](int from, int to) mutable {
    if (is_first) {
      std::cout << to + 1 << ' ';
      is_first = false;
    }
    std::cout << from + 1 << ' ';
  });
  std::cout << std::endl;

  return 0;
}
