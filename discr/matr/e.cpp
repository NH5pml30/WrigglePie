/* Kholiavin Nikolai, M3138 */
#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <set>
#include <vector>
#include <stack>
#include <memory>
#include <fstream>
#include <map>

int main()
{
#ifdef _DEBUG
  auto &in = std::cin;
  auto &out = std::cout;
#else
  auto in = std::ifstream("cycles.in");
  auto out = std::ofstream("cycles.out");
#endif

  int n, m;
  in >> n >> m;

  std::multimap<int, int, std::greater<int>> w2id;
  for (int i = 0; i < n; i++)
  {
    int w;
    in >> w;
    w2id.insert({w, i});
  }

  std::vector<std::vector<int32_t>> el2cycles(n);
  for (int i = 0; i < m; i++)
  {
    int k;
    in >> k;
    int32_t cycle = 0;
    for (int j = 0; j < k; j++)
    {
      int el;
      in >> el;
      --el;
      cycle |= 1 << el;
    }
    for (int j = 0; j < n; j++)
      if (cycle & (1 << j))
        el2cycles[j].push_back(cycle);
  }

  int32_t ans = 0;
  int ans_w = 0;
  for (auto &w_el : w2id)
  {
    ans |= (1 << w_el.second);
    bool isok = true;
    for (auto cycle : el2cycles[w_el.second])
      if ((cycle & ans) == cycle)
      {
        isok = false;
        break;
      }
    if (!isok)
      ans &= ~(1 << w_el.second);
    else
      ans_w += w_el.first;
  }

  out << ans_w << std::endl;

  return 0;
}
