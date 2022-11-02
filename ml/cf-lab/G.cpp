#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
#include <numbers>
#include <numeric>
#include <ranges>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>
#include <random>
#include <set>
#include <map>
#include <span>

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int arity;
  std::cin >> arity;
  std::vector<int> the_args;
  int sign = 1;
  {
    std::vector<int> truthy_args;
    std::vector<int> falsy_args;

    for (int i = 0; i < (1 << arity); i++)
    {
      int val;
      std::cin >> val;
      if (val)
        truthy_args.push_back(i);
      else
        falsy_args.push_back(i);
    }

    if (truthy_args.size() > falsy_args.size())
    {
      truthy_args.swap(falsy_args);
      sign = -1;
    }
    the_args.swap(truthy_args);
  }

  if (the_args.empty())
  {
    std::cout << 1 << '\n' << 1 << '\n';
    for (int i = 0; i < arity; i++)
      std::cout << 0 << ' ';
    std::cout << -sign << std::endl;
    return 0;
  }

  std::cout << 2 << '\n' << the_args.size() << ' ' << 1 << '\n';
  for (int args : the_args)
  {
    int bias = 0;
    for (int i = 0; i < arity; i++)
    {
      if ((args >> i) & 1)
      {
        std::cout << -1 << ' ';
        bias += 1;
      }
      else
      {
        std::cout << 1 << ' ';
      }
    }
    std::cout << bias - 0.5 << '\n';
  }

  for (int i = 0; i < (int)the_args.size(); i++)
    std::cout << -sign << ' ';
  std::cout << sign * (the_args.size() - 0.5) << std::endl;
  return 0;
}
