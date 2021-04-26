/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <list>
#include <algorithm>
#include <set>

#define MY_ASSERT(expr) \
  if (!(expr))  \
    throw "up"  \

int main()
{
  int n;
  std::cin >> n;

  std::set<int> leaves;
  std::vector<std::set<int>> adj(n);
  for (int i = 0; i < n - 1; i++)
  {
    int u, v;
    std::cin >> u >> v;
    u--, v--;
    adj[u].insert(v);
    adj[v].insert(u);
  }

  for (int i = 0; i < n; i++)
    if (adj[i].size() == 1)
      leaves.insert(i);

  for (int i = 0; i < n - 2; i++)
  {
    int leaf = *leaves.begin(), p = *adj[leaf].begin();
    leaves.erase(leaves.begin());
    std::cout << p + 1 << ' ';
    adj[p].erase(leaf);
    adj[leaf].clear();
    if (adj[p].size() == 1)
      leaves.insert(p);
  }
  std::cout << std::endl;

  return 0;
}
