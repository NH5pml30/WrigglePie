#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <numeric>

template<class Callback>
void gen_subsets(std::vector<bool> &was, size_t cur, Callback &&callback)
{
  if (cur == was.size())
  {
    callback(was);
    return;
  }

  was[cur] = false;
  gen_subsets(was, cur + 1, std::forward<Callback>(callback));
  was[cur] = true;
  gen_subsets(was, cur + 1, std::forward<Callback>(callback));
}

template<class Callback>
void gen_subsets(size_t n, Callback &&callback)
{
  std::vector<bool> was(n);
  gen_subsets(was, 0, std::forward<Callback>(callback));
}

template<class Cont, class Callback>
std::enable_if_t<!std::is_same_v<Cont, size_t>> gen_subsets(Cont cont, Callback &&callback)
{
  std::vector<bool> was(cont.size());
  gen_subsets(cont.size(), [&](std::vector<bool> &was) {
    Cont subset = Cont();
    size_t i = 0;
    for (auto &el : cont)
      if (was[i++])
        subset.insert(subset.end(), el);
    callback(std::move(subset));
  });
}

template<class Cont>
std::vector<Cont> gen_subsets(Cont cont)
{
  std::vector<bool> was(cont.size());
  std::vector<Cont> res;
  gen_subsets(cont, [&](Cont &&subset) { res.emplace_back(std::move(subset)); });
  return res;
}

std::set<int> range(int max)
{
  std::set<int> res;
  for (int i = 0; i < max; i++)
    res.insert(i);
  return res;
}

int main()
{
  constexpr int n = 5;
  std::vector<std::set<int>> subsets = gen_subsets(range(n));
  size_t max_n_of_bases = 0;
  int counter = 0;
  gen_subsets(subsets, [&](std::vector<std::set<int>> I) {
    counter++;
    if (counter % 10000 == 0)
      std::cout << '\r' << counter * 100.0 / (1ull << (1ull << n)) << "%";

    if (I.empty() || !I[0].empty())
      return;
    size_t n_of_bases = 0;
    for (size_t i = 0; i < I.size(); i++)
    {
      std::vector<std::set<int>> subsets = gen_subsets(I[i]);
      for (auto &subset : subsets)
        if (std::find(I.begin(), I.end(), subset) == I.end())
          return;

      bool is_base = true;
      for (size_t j = 0; j < I.size(); j++)
        if (i != j)
        {
          bool is_subset = true;
          for (auto el : I[i])
            if (I[j].count(el) == 0)
            {
              is_subset = false;
              break;
            }
          if (is_subset)
          {
            is_base = false;
            break;
          }
        }

      n_of_bases += is_base;
    }

    max_n_of_bases = std::max(max_n_of_bases, n_of_bases);
  });

  std::cout << max_n_of_bases << std::endl;
  return 0;
}
