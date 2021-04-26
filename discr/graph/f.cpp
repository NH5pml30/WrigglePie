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

  std::list<int> p;
  std::vector<int> v2deg(n);
  std::set<int> not_p;
  for (int i = 0; i < n; i++)
    not_p.insert(i);
  for (int i = 0; i < n - 2; i++)
  {
    int pi;
    std::cin >> pi;
    pi--;
    p.push_back(pi);
    not_p.erase(pi);
    v2deg[pi]++;
  }

  for (int i = 0; i < n - 2; i++)
  {
    int u = *not_p.begin();
    not_p.erase(*not_p.begin());
    int v = p.front();
    p.pop_front();
    std::cout << u + 1 << ' ' << v + 1 << std::endl;
    if (--v2deg[v] == 0)
      not_p.insert(v);
  }
  int u = *not_p.begin(), v = *--not_p.end();
  std::cout << u + 1 << ' ' << v + 1 << std::endl;
  return 0;
}
