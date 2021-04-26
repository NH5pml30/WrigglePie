#include <iostream>
#include <list>
#include <vector>
#include <unordered_set>

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

  // tear_at will be at front
  void rotate(const_iterator new_begin, const_iterator tear_left)
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
      rotate(new_begin, std::prev(tear_left));
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
          g.mat[i][j] = true;
          g.mat[j][i] = true;
        }
      }

    return in;
  }
};

void update_neighbours(graph &g, path<int> &p, std::unordered_set<int> &in_path,
                       std::unordered_set<int> &potential_neighbours, size_t v)
{
  potential_neighbours.erase((int)v);
  for (size_t i = 0; i < g.size(); i++)
    if (g.has_edge(v, i) && !in_path.count(i))
      potential_neighbours.insert((int)i);
  in_path.insert((int)v);
}

void grow_path(graph &g, path<int> &p, std::unordered_set<int> &in_path,
               std::unordered_set<int> &potential_neighbours)
{
  int s = p.front(), t = p.back(), v = -1;

  for (auto u : potential_neighbours)
    if (g.has_edge(u, s))
    {
      p.push_front(u);
      v = u;
      break;
    }
    else if (g.has_edge(t, u))
    {
      p.push_back(u);
      v = u;
      break;
    }

  if (v == -1)
  {
    // ends are occupied
    v = potential_neighbours.empty() ? t : *potential_neighbours.begin();
    auto append_at = p.end(), tear_at = p.end();
    auto next = std::next(p.begin()), prev = p.begin();
    if (g.has_edge(v, *prev))
      append_at = prev;

    for (; next != p.end(); prev = next, ++next)  // n > 3 => here |p| > 1
    {
      if (append_at == p.end() && g.has_edge(v, *next))
        append_at = next;

      if (tear_at == p.end() && g.has_edge(s, *next) && g.has_edge(*prev, t))
      {
        // found where to tear
        tear_at = prev;
        if (v == t)
          append_at = tear_at;
      }
    }

    p.rotate(append_at, tear_at);
    p.push_front(v);
  }

  update_neighbours(g, p, in_path, potential_neighbours, v);
}

path<int> gamiltonian_cycle(graph &g)
{
  path<int> res({0});
  std::unordered_set<int> in_path, potential_neighbours;
  update_neighbours(g, res, in_path, potential_neighbours, 0);

  for (size_t i = 0; i < g.size(); i++)
    grow_path(g, res, in_path, potential_neighbours);
  return res;
}

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  graph g;
  std::cin >> g;

  path<int> res = gamiltonian_cycle(g);
  auto end = res.end();

  for (auto it = res.begin(); it != std::prev(res.end()); it++)
    std::cout << *it + 1 << ' ';
  std::cout << std::endl;

  return 0;
}
