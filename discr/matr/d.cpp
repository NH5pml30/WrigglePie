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

struct set
{
  int16_t data;

  explicit set(int16_t data = 0) : data(data) {}

  set &add(int el)
  {
    data |= 1 << el;
    return *this;
  }

  bool find(int el) const
  {
    return data & (1 << el);
  }

  set operator-(set other) const
  {
    return set(data & ~other.data);
  }

  int size() const
  {
    int ans = 0;
    for (int i = 0; i < sizeof(int16_t) * CHAR_BIT; i++)
      ans += (data >> i) & 1;
    return ans;
  }

  template<class Callback>
  void generate_subsets(Callback &&callback)
  {
    int16_t subset = data;
    while (subset > 0)
    {
      if (callback(set(subset)))
        break;
      subset = (subset - 1) & data;
    }
  }

  bool operator<(set other) const
  {
    return data < other.data;
  }
};

int main()
{
#ifdef _DEBUG
  auto &in = std::cin;
  auto &out = std::cout;
#else
  auto in = std::ifstream("check.in");
  auto out = std::ofstream("check.out");
#endif

  int n, m;
  in >> n >> m;

  std::set<set> I;
  for (int i = 0; i < m; i++)
  {
    int k;
    in >> k;
    set s;
    for (int j = 0; j < k; j++)
    {
      int el;
      in >> el;
      s.add(el - 1);
    }
    I.insert(s);
  }

  if (!I.count(set()))
  {
    out << "NO" << std::endl;
    return 0;
  }

  for (auto A : I)
  {
    bool is_ok = true;
    A.generate_subsets([&](set B) {
      if (!I.count(B))
      {
        is_ok = false;
        return true;
      }
      return false;
    });
    if (!is_ok)
    {
      out << "NO" << std::endl;
      return 0;
    }
  }

  for (auto A : I)
    for (auto B : I)
      if (B.size() < A.size())
      {
        set diff = A - B;
        bool found = false;
        for (int i = 0; i < n; i++)
          if (diff.find(i) && I.count(set(B).add(i)))
          {
            found = true;
            break;
          }
        if (!found)
        {
          out << "NO" << std::endl;
          return 0;
        }
      }

  out << "YES" << std::endl;

  return 0;
}
