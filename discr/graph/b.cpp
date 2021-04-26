#include <iostream>
#include <list>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <numeric>
#include <functional>

template<typename T>
class path
{
private:
  std::list<T> data;

public:
  path() = default;
  path(std::list<T> data) : data(std::move(data)) {}

  using iterator = typename std::list<T>::iterator;
  using const_iterator = typename std::list<T>::const_iterator;

  T &front()
  {
    return data.front();
  }
  const T &front() const
  {
    return data.front();
  }

  T &back()
  {
    return data.back();
  }
  const T &back() const
  {
    return data.back();
  }

  size_t size() const
  {
    return data.size();
  }

  const_iterator begin() const
  {
    return data.begin();
  }
  iterator begin()
  {
    return data.begin();
  }

  const_iterator end() const
  {
    return data.end();
  }
  iterator end()
  {
    return data.end();
  }

  void push_back(T val)
  {
    data.push_back(std::move(val));
  }
  void push_front(T val)
  {
    data.push_front(std::move(val));
  }

  void pop_back()
  {
    data.pop_back();
  }
  void pop_front()
  {
    data.pop_front();
  }

  void reverse()
  {
    data.reverse();
  }

  // new_begin will be at front
  void rotate(const_iterator new_begin)
  {
    if (new_begin != data.begin())
      data.splice(data.begin(), data, new_begin, data.end());
  }

  // tear_at will be at front
  void split_and_glue(const_iterator new_begin, const_iterator tear_left)
  {
    auto it = new_begin;
    for (; it != end() && it != tear_left; it++)
      ;
    bool left_from_tear = it == tear_left;

    std::list<T> new_path;

    if (left_from_tear)
    {
      // save iterator to right tear in case tear_left == new_begin and is removed
      if (tear_left == end())
        throw "up";
      auto tear_right = std::next(tear_left);

      if (new_begin == end())
        throw "up";
      new_path.splice(new_path.end(), data, data.begin(), std::next(new_begin));
      new_path.reverse();

      new_path.splice(new_path.end(), data, tear_right, data.end());

      data.reverse();
      new_path.splice(new_path.end(), data);

      data = std::move(new_path);
    }
    else
    {
      data.reverse();
      if (tear_left == begin())
        throw "up";
      split_and_glue(new_begin, std::prev(tear_left));
    }
  }
};

struct graph
{
  std::vector<std::vector<bool>> mat;

  void resize(size_t new_size)
  {
    mat.resize(new_size);
    for (size_t i = 0; i < new_size; i++)
      mat[i].resize(new_size);
  }

  size_t size() const
  {
    return mat.size();
  }

  bool has_edge(size_t from, size_t to) const
  {
    return mat[from][to];
  }

  void add_edge(size_t from, size_t to)
  {
    mat[from][to] = true;
  }

  void remove_edge(size_t from, size_t to)
  {
    mat[from][to] = false;
  }

  friend std::istream &operator>>(std::istream &in, graph &g)
  {
    size_t n;
    in >> n;

    g.resize(n);

    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < i; j++)
      {
        char e;
        in >> e;
        if (e == '1')
        {
          g.add_edge(i, j);
          g.add_edge(j, i);
        }
      }

    return in;
  }
};

struct gamiltonian_searcher
{
private:
  struct hasher
  {
    int n;

    hasher(int n) : n(n) {}

    int operator()(const std::pair<int, int> &p) const
    {
      return p.first * n + p.second;
    }
  };

  graph &g;
  mutable std::unordered_set<int>
    in_path,
    potential_neighbours;
  mutable std::unordered_set<std::pair<int, int>, hasher> absent_edges;
  mutable std::vector<int> v2deg;

  path<int> check_cycle(path<int> p) const
  {
    std::vector<bool> was(g.size());
    if (p.size() != g.size())
      throw "up";
    for (auto it = p.begin(); it != p.end(); it++)
    {
      auto next = std::next(it);
      if (next == p.end())
        next = p.begin();
      else if (was[*it])
        throw "up";
      was[*it] = true;
      if (!g.has_edge(*it, *next))
        throw "up";
    }
    return p;
  }

  path<int> search() const
  {
    if (absent_edges.empty())
    {
      // full graph
      std::list<int> a;
      a.resize(g.size());
      std::iota(a.begin(), a.end(), 0);
      return path<int>(std::move(a));
    }
    else
    {
      int max_sum = -1;
      auto max_edge = absent_edges.end();
      for (auto it = absent_edges.begin(); it != absent_edges.end(); it++)
        if (v2deg[it->first] + v2deg[it->second] > max_sum)
        {
          max_sum = v2deg[it->first] + v2deg[it->second];
          max_edge = it;
        }

      // add edge u <-> v
      int u = max_edge->first, v = max_edge->second;
      absent_edges.erase(max_edge);
      g.add_edge(u, v);
      g.add_edge(v, u);
      v2deg[u]++;
      v2deg[v]++;

      // find gamiltonian cycle in this graph
      path<int> cycle = search();

      // remove edge u <-> v
      bool uses_uv = true;
      {
        auto it = std::find(cycle.begin(), cycle.end(), u);
        auto prev = it == cycle.begin() ? --cycle.end() : std::prev(it);
        auto next = it == --cycle.end() ? cycle.begin() : std::next(it);

        if (*next == v)
        {
          cycle.reverse();
          prev = it == cycle.begin() ? --cycle.end() : std::prev(it);
          next = it == --cycle.end() ? cycle.begin() : std::next(it);
        }
        if (*prev == v)
          cycle.rotate(it);
        else
          uses_uv = false;
      }
      v2deg[u]--;
      v2deg[v]--;
      g.remove_edge(u, v);
      g.remove_edge(v, u);
      absent_edges.insert(std::minmax({u, v}));

      if (!uses_uv)
        return check_cycle(cycle);

      // tear and glue back together the path, creating a cycle without edge (u, v)
      auto tear_at = cycle.end();
      for (auto prev = cycle.begin(), next = std::next(prev); next != cycle.end(); prev = next, ++next)
      {
        if (tear_at == cycle.end() && g.has_edge(u, *next) && g.has_edge(*prev, v))
          // found where to tear
          tear_at = prev;
      }
      if (tear_at == cycle.end())
        throw "up";

      cycle.split_and_glue(tear_at, tear_at);
      return check_cycle(cycle);
    }
  }

public:
  gamiltonian_searcher(graph &g) : g(g), v2deg(g.size()), absent_edges(0, hasher((int)g.size()))
  {
    std::vector<int> d;
    for (size_t i = 0; i < g.size(); i++)
    {
      size_t deg = g.size() - 1;
      for (size_t j = 0; j < g.size(); j++)
        if (i != j && !g.has_edge(i, j))
        {
          absent_edges.insert(std::minmax({(int)i, (int)j}));
          deg--;
        }
      v2deg[i] = deg;
      d.push_back(deg);
    }

    std::sort(d.begin(), d.end());
    for (size_t k = 1; k < (g.size() + 1) / 2; k++)
      if (d[k - 1] <= k && d[g.size() - k - 1] < g.size() - k)
        throw "up";
  }

  path<int> operator()() const
  {
    return search();
  }
};

graph build_graph(std::vector<int> d)
{
  std::vector<std::pair<int, int>> dd;
  for (auto deg : d)
    dd.push_back({deg, (int)dd.size()});
  std::sort(dd.begin(), dd.end(), std::greater<>());

  graph g;
  g.resize(d.size());

  while (dd[0].first > 0)
  {
    size_t i;
    for (i = 1; i < dd.size() && dd[0].first > 0; i++)
    {
      // if (dd[i].first == 0)
      //   throw "up";
      if (dd[i].first == 0)
        break;
      dd[0].first--;
      dd[i].first--;
      g.add_edge(dd[0].second, dd[i].second);
      g.add_edge(dd[i].second, dd[0].second);
    }

    for (; i < dd.size() && dd[0].first > 0; i++)
      if (!g.has_edge(dd[0].second, dd[i].second))
      {
        dd[0].first--;
        dd[i].first--;
        g.add_edge(dd[0].second, dd[i].second);
        g.add_edge(dd[i].second, dd[0].second);
      }

    if (dd[0].first > 0)
      throw "up";

    std::sort(dd.begin(), dd.end(), std::greater<>());
  }

  return g;
}

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  // std::vector<int> d(6);
  // std::iota(d.begin(), d.end(), 1);
  // d = {2, 2, 4, 4, 5, 5};
  graph g;// = build_graph(d);
  std::cin >> g;

  path<int> res = gamiltonian_searcher(g)();

  for (auto v : res)
    std::cout << v + 1 << ' ';
  std::cout << std::endl;

  return 0;
}
