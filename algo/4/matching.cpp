/* Nikolai Kholiavin, M3138 */
#include <fstream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cstring>

int main() {
  std::string name = "matching";
  std::ifstream in(name + ".in");
  std::ofstream out(name + ".out");

  int n;
  in >> n;

  struct NODE {
    std::vector<std::pair<NODE *, int>> Adjoint;
    long long MaxWoChildren = -1, MaxWithChild = -1;
  };

  std::vector<NODE> nodes(n);
  for (int i = 0; i < n - 1; i++) {
    int start, end, weight;
    in >> start >> end >> weight;
    start--, end--;
    nodes[start].Adjoint.push_back({&nodes[end], weight});
    nodes[end].Adjoint.push_back({&nodes[start], weight});
  }

  std::vector<std::pair<NODE *, NODE *>> stack;
  stack.reserve(n);
  stack.push_back({&nodes[0], nullptr});
  while (!stack.empty()) {
    auto[node, last] = stack.back();
    bool ready = true;
    long long sum_max = 0;
    for (auto ch : node->Adjoint)
      if (ch.first != last) {
        if (ch.first->MaxWithChild < 0) {
          ready = false;
          stack.push_back({ch.first, node});
        }
        sum_max += std::max(ch.first->MaxWithChild, ch.first->MaxWoChildren);
      }

    if (ready) {
      long long max = 0;
      for (auto ch : node->Adjoint)
        if (ch.first != last)
          max = std::max(max, sum_max -
            std::max(ch.first->MaxWithChild,
                     ch.first->MaxWoChildren) +
            ch.first->MaxWoChildren + ch.second);
      node->MaxWithChild = max;
      node->MaxWoChildren = sum_max;
      stack.pop_back();
    }
  }

  out << std::max(nodes[0].MaxWoChildren, nodes[0].MaxWithChild) << std::endl;

  return 0;
}
