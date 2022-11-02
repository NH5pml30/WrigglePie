#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <numbers>
#include <numeric>
#include <random>
#include <ranges>
#include <set>
#include <span>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>
#include <fstream>
#include <random>

struct gen_bit
{
  uint32_t accum;
  int accum_i = 32;
  std::default_random_engine eng{};

  void regen()
  {
    accum = eng();
    accum_i = 0;
  }

  gen_bit()
  {
    regen();
  }

  bool operator()()
  {
    if (accum_i == 32)
      regen();
    return (accum >> accum_i++) & 1;
  }
};

template<template<typename> typename T = std::vector>
auto materialize(std::ranges::range auto &&range)
{
  auto r = range | std::views::common;
  return T<std::ranges::range_value_t<decltype(r)>>(std::begin(r), std::end(r));
}

struct nn
{
  struct layer
  {
    struct neuron
    {
      std::vector<double> ws;

      bool operator()(const std::vector<bool> &prev)
      {
        double val = std::transform_reduce(ws.begin(), --ws.end(), prev.begin(), 0) + ws.back();
        if (val > 0)
          return true;
        else if (val < 0)
          return false;
        else
          throw "up";
      }
    };

    std::vector<neuron> ns;

    std::vector<bool> operator()(std::vector<bool> &prev)
    {
      return materialize(ns | std::views::transform([&prev](auto &n) { return n(prev); }));
    }
  };

  std::vector<layer> ls;

  std::vector<bool> operator()(std::vector<bool> prev)
  {
    for (auto &l : ls)
      prev = l(prev);
    return prev;
  }
};

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);
  gen_bit gen;

  for (int M = 4; M <= 5; M++)
  {
    for (int i = 0; i < 500; i++)
    {
      std::vector<bool> table;
      {
        std::ofstream in("G.in");
        in << M << '\n';
        table.reserve(1 << M);
        for (int i = 0; i < (1 << M); i++)
        {
          table.push_back(gen());
          in << table.back() << '\n';
        }
      }

      std::system("G.exe < G.in > G.out");

      nn net;
      {
        std::ifstream out("G.out");
        int D;
        out >> D;
        std::vector<int> sizes(D);
        for (auto &size : sizes)
          out >> size;
        net.ls.resize(D);
        int last_size = M;
        for (int i = 0; i < D; i++)
        {
          net.ls[i].ns.resize(sizes[i]);
          for (auto &n : net.ls[i].ns)
          {
            n.ws.resize(last_size + 1);
            for (auto &w : n.ws)
              out >> w;
          }
          last_size = sizes[i];
        }
      }

      for (int a = 0; a < (int)table.size(); a++)
      {
        std::vector<bool> args(M);
        for (int i = 0; i < M; i++)
          if ((a >> i) & 1)
            args[i] = true;
        double val_to_check = net(args)[0];
        if (val_to_check != table[a])
          throw "up";
      }
    }
  }

  return 0;
}
