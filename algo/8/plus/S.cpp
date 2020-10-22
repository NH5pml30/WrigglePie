/* Kholiavin Nikolai, M3138 */
#include <functional>
#include <iostream>
#include <numeric>
#include <vector>
#include <list>
#include <set>
#include <stack>
#include <algorithm>
#include <ios>

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n, l;
  std::cin >> n >> l;

  std::vector<int> output(l);
  std::vector<int> last_occ(n, -1);
  for (int i = 0; i < l; i++) {
    std::cin >> output[i];
    last_occ[--output[i]] = i;
  }

  std::vector<std::pair<int, int>> edges;
  std::vector<int> path_to_root;

  std::function<int(int, int)> inv_dfs = [&](int begin, int end) {
    int vertex = output[begin];
    for (int i = 0; i < (int)path_to_root.size() - 1; i++)
      if (vertex > path_to_root[i + 1])
        edges.push_back({vertex, path_to_root[i]});

    path_to_root.push_back(vertex);
    for (int i = begin + 1; i < end; i++) {
      edges.push_back({vertex, output[i]});
      i = inv_dfs(i, last_occ[output[i]] + 1);
    }
    path_to_root.pop_back();
    return end;
  };

  int i = 0;
  while (i < l)
    i = inv_dfs(i, last_occ[output[i]] + 1);

  std::cout << edges.size() << std::endl;
  for (auto edge : edges)
    std::cout << edge.first + 1 << ' ' << edge.second + 1 << std::endl;


  return 0;
}
