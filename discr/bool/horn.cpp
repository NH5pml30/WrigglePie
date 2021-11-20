/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <list>
#include <set>
#include <algorithm>

int main()
{
  int n, k;
  std::cin >> n >> k;

  std::vector<std::set<int>> data(k);
  std::vector<std::set<int> *> closes(k);

  for (int i = 0; i < k; i++)
  {
    closes[i] = &data[i];
    for (int j = 0; j < n; j++)
    {
      int x;
      std::cin >> x;
      if (x != -1)
        closes[i]->insert((j + 1) * (x * 2 - 1));
    }
  }

  while (true)
  {
    size_t end = std::partition(closes.begin(), closes.end(), []( std::set<int> *S )
      {
        return S->size() == 1;
      }) - closes.begin();

    if (end == 0)
      break;

    for (size_t i = 0; i < end; i++)
    {
      int ind = *closes[i]->begin();
      for (size_t j = i + 1; j < closes.size(); j++)
      {
        if (closes[j]->count(ind) != 0)
        {
          if (j < end)
            end--;
          closes.erase(closes.begin() + j);
          j--;
        }
        else if (auto place = closes[j]->find(-ind); place != closes[j]->end())
        {
          closes[j]->erase(place);
          if (closes[j]->size() == 0)
          {
            std::cout << "YES" << std::endl;
            return 0;
          }
        }
      }
    }

    closes.erase(closes.begin(), closes.begin() + end);
  }

  std::cout << "NO" << std::endl;
  return 0;
}
