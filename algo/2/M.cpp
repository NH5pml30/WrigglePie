#include <iostream>
#include <vector>

int main() {
  int n, m, k;
  std::cin >> n >> m >> k;
  std::vector<std::pair<int, int>> comps(m);
  std::vector<int> layout(k + 1);

  for (int i = 0; i < k; i++) {
    std::cin >> layout[i + 1];
    layout[i + 1] += layout[i];
    for (int j = layout[i]; j < layout[i + 1]; j++) {
      std::cin >> comps[j].first >> comps[j].second;
      if (--comps[j].first > --comps[j].second)
        std::swap(comps[j].first, comps[j].second);
    }
  }

  int last = 1 << n;
  bool isok = true;
  for (int seq = 0; seq < last; seq++) {
    int sort_seq = seq;
    for (int layer = 0; layer < k; layer++)
      for (int comp = layout[layer]; comp < layout[layer + 1]; comp++) {
        int
          x1 = (sort_seq >> comps[comp].first) & 1,
          x2 = (sort_seq >> comps[comp].second) & 1;
        sort_seq &= ~(!x2 << comps[comp].first);
        sort_seq |= (x1 << comps[comp].second);
      }

    for (int i = 0; i < n - 1; i++)
      if (((sort_seq >> i) & 1) && !((sort_seq >> (i + 1)) & 1)) {
        isok = false;
        break;
      }
    if (!isok)
      break;
  }
  std::cout << (isok ? "Yes" : "No") << std::endl;

  return 0;
}
