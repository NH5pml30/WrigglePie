/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <list>
#include <algorithm>
#include <set>
#include <functional>
#include <queue>

#define MY_ASSERT(expr) \
  if (!(expr))  \
    throw "up"  \

int main()
{
  int n, m;
  std::cin >> n >> m;

  struct node
  {
    std::vector<short> adj;
    std::set<short> potential_colors;
    int color = -2;
  };

  std::vector<node> nodes(n);

  for (int i = 0; i < m; i++)
  {
    int u, v;
    std::cin >> u >> v;
    u--, v--;
    nodes[u].adj.push_back(v);
    nodes[v].adj.push_back(u);
  }

  int min_deg_i = 0, max_deg_i = 0;
  for (int i = 0; i < n; i++)
  {
    if (nodes[i].adj.size() < nodes[min_deg_i].adj.size())
      min_deg_i = i;
    if (nodes[i].adj.size() > nodes[max_deg_i].adj.size())
      max_deg_i = i;
  }

  int k = (int)nodes[max_deg_i].adj.size() | 1;

  std::set<short> all;
  for (int i = 0; i < k; i++)
    all.insert(i);

  // min_deg < k

  // bfs
  auto bfs = [&](int start) {
    std::vector<short> vs;
    std::queue<short> q;
    q.push(start);
    while (!q.empty())
    {
      int v = q.front();
      nodes[v].potential_colors = all;
      q.pop();
      vs.push_back(v);
      for (auto child : nodes[v].adj)
        if (nodes[child].color == -2)
        {
          nodes[child].color = -1;
          q.push(child);
        }
    }

    for (auto vit = vs.rbegin(); vit != vs.rend(); vit++)
    {
      int v = *vit;

      nodes[v].color = *nodes[v].potential_colors.begin();
      for (auto u : nodes[v].adj)
        nodes[u].potential_colors.erase(nodes[v].color);
    }
  };

  for (int i = 0; i < n; i++)
    if (nodes[i].adj.size() < k && nodes[i].color == -2)
    {
      nodes[i].color = -1;
      bfs(i);
    }

  std::cout << k << std::endl;

  for (int i = 0; i < n; i++)
    std::cout << nodes[i].color + 1 << '\n';
  std::cout.flush();
  return 0;
}
