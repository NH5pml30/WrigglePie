/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <list>
#include <algorithm>

#define MY_ASSERT(expr) \
  if (!(expr))  \
    throw "up"  \

template<typename type>
void rotate(std::list<type> &list, typename std::list<type>::const_iterator new_begin)
{
  if (new_begin == list.begin())
    return;
  list.splice(list.begin(), list, new_begin, list.end());
}

template<typename Iter, typename type, class Compare>
Iter find_place(Iter begin, Iter end, Compare &&less, type val)
{
  if (begin == end)
    return begin;

  auto next = begin;
  for (; next != end && less(*next, val); ++next)
    ;
  return next;
}

int main()
{
  int N;
  std::cin >> N;

  std::vector<std::vector<bool>> less(N);
  for (int i = 0; i < N; i++)
  {
    less[i].resize(N);
    for (int j = 0; j < i; j++)
    {
      char ch;
      std::cin >> ch;
      less[i][j] = ch == '1';
      less[j][i] = ch == '0';
    }
  }

  // build gamiltonian path
  std::list<int> path;

  auto compare = [&](int vi, int vj) { return less[vi][vj]; };

  for (int i = 0; i < N; i++)
    path.insert(find_place(path.begin(), path.end(), compare, i), i);

  for (auto it = path.begin(); it != --path.end(); it++)
    MY_ASSERT(compare(*it, *std::next(it)));

  // build gamiltonian cycle
  std::list<int> stub;
  stub.splice(stub.begin(), path, path.begin(), std::find_if(path.begin(), path.end(), [&](int vi) {
                return less[path.back()][vi];
              }));
  std::list<int> cycle = std::move(path);

  auto check_cycle = [&] {
    for (auto it = cycle.begin(); it != --cycle.end(); it++)
      MY_ASSERT(compare(*it, *std::next(it)));
    MY_ASSERT(compare(cycle.back(), cycle.front()));
  };

  check_cycle();

  while (!stub.empty())
  {
    auto split =
        std::find_if(cycle.begin(), cycle.end(), [&](int vk) { return less[vk][stub.back()]; });
    if (split == cycle.end())
    {
      auto vj = stub.end(), vt = cycle.end();

      for (vj = stub.begin(); vj != stub.end(); ++vj)
        for (vt = cycle.begin(); vt != cycle.end(); ++vt)
          if (less[*vt][*vj])
            goto double_break;
double_break:

      MY_ASSERT(vj != stub.end() && vj != --stub.end() && vt != cycle.end());
      rotate(cycle, std::next(vt)); // vt will be at the end
      cycle.splice(cycle.begin(), stub, vj, stub.end()); // loop with vt < vj
    }
    else
    {
      cycle.splice(find_place(split, cycle.end(), compare, stub.back()), stub,
                   --stub.end(), stub.end());
      check_cycle();
    }
  }

  for (auto el : cycle)
    std::cout << el
#ifndef _DEBUG
      + 1
#endif
      << ' ';
  std::cout << std::endl;

  return 0;
}
