/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <cassert>
#include <sstream>
#include <functional>
#include <ios>
#include <iostream>
#include <list>
#include <numeric>
#include <set>
#include <stack>
#include <string>
#include <vector>
#include <map>

struct empty {};

template <class NODE_DATA = empty, class EDGE_DATA = empty>
class graph
{
public:
  struct NODE
  {
    struct EDGE_ENTRY;

    int id;
    NODE_DATA data;
    std::set<EDGE_ENTRY, std::less<>> adjacent;

    NODE(int id = -1) : id(id) {}
  };

  enum class color { white, gray, black };
  std::vector<color> cols;

private:
  std::vector<NODE> nodes;
  size_t n_of_edges = 0;

public:
  class NODE_PROXY
  {
    friend class EDGE_PROXY;

  public:
    const int id;
    NODE_DATA &data;
    const color &col;

    // private:
    static NODE_DATA null_data;

    bool is_valid() const { return &data != &null_data; }

    NODE_PROXY(graph &g, int id)
      : id(id),
      data(id >= 0 ? g.nodes[id].data : null_data),
      col(id >= 0 ? g.cols[id] : static_cast<const color &>(color()))
    {}
  };

  class EDGE_PROXY
  {
    friend class graph;

  public:
    const NODE_PROXY from, to;
    const EDGE_DATA &edge_data;

    // private:
    static EDGE_DATA null_data;

    EDGE_PROXY(graph &g, int from, const typename NODE::EDGE_ENTRY &entry)
      : from(g, from), to(g, entry.other_id), edge_data(entry.data)
    {}

    EDGE_PROXY(graph &g, int to)
      : from(g, -1), to(g, to), edge_data(null_data)
    {}
  };

  void exec_dfs(
    const std::function<bool(EDGE_PROXY from_parent)> &before,
    const std::function<void(EDGE_PROXY from_parent,
      const std::vector<EDGE_PROXY> &to_children)>
    &after,
    EDGE_PROXY from_parent)
  {
    int node_id = from_parent.to.id;
    if (before(from_parent) || cols[node_id] != color::white) return;
    cols[node_id] = color::gray;
    std::vector<EDGE_PROXY> to_children;
    for (auto &next : nodes[node_id].adjacent)
    {
      EDGE_PROXY to_child = EDGE_PROXY(*this, node_id, next);
      exec_dfs(before, after, to_child);
      to_children.push_back(to_child);
    }
    after(from_parent, to_children);
    cols[node_id] = color::black;
  }

  graph() {}

  void resize(size_t size)
  {
    size_t start = nodes.size();
    nodes.resize(size);
    cols.resize(size);
    for (size_t i = start; i < size; i++) nodes[i].id = (int)i;
  }

  graph reverse()
  {
    graph res;
    res.resize(size());
    for (auto &n : nodes)
      for (auto &e : n.adjacent) res.add_edge(e.other_id, n.id, e.data);
    return res;
  }

  size_t size() const { return nodes.size(); }
  size_t n_edges() const { return n_of_edges; }

  void clear()
  {
    nodes.clear();
    cols.clear();
  }

  void add_edge(int from, int to, const EDGE_DATA &data = {})
  {
    if (from != to)
      n_of_edges += nodes[from].adjacent.insert(NODE::EDGE_ENTRY(to, data)).second;
  }
  bool exists_edge(int from, int to)
  {
    return nodes[from].adjacent.count(to);
  }
  graph & remove_edge(int from, int to)
  {
    auto it = nodes[from].adjacent.find(to);
    if (it != nodes[from].adjacent.end())
    {
      nodes[from].adjacent.erase(it);
      n_of_edges--;
    }
    return *this;
  }

  void dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
    const std::function<void(EDGE_PROXY from_parent,
      const std::vector<EDGE_PROXY> &to_children)>
    &after,
    int start_at)
  {
    std::fill(cols.begin(), cols.end(), color::white);
    exec_dfs(before, after, EDGE_PROXY(*this, start_at));
  }

  void dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
    const std::function<void(EDGE_PROXY from_parent,
      const std::vector<EDGE_PROXY> &to_children)>
    &after,
    const std::function<void()> &start_callback,
    const std::function<void()> &end_callback)
  {
    std::fill(cols.begin(), cols.end(), color::white);
    for (int i = 0; i < (int)nodes.size(); i++)
      if (cols[i] == color::white)
      {
        start_callback();
        exec_dfs(before, after, EDGE_PROXY(*this, i));
        end_callback();
      }
  }

  graph compress_edge(int u, int v)
  {
    if (!exists_edge(u, v))
      return *this;
    if (v < u)
      std::swap(u, v);

    auto get_new_id = [&](int x)
    {
      if (x < v)
        return x;
      if (x == v)
        return u;
      return x - 1;
    };

    // delete v, merge edges
    graph res;
    res.resize(size() - 1);
    for (auto &node : nodes)
      for (auto &adj : node.adjacent)
        res.add_edge(get_new_id(node.id), get_new_id(adj.other_id), adj.data);
    return res;
  }

  void print_undirected(std::ostream &o)
  {
    // o << size() << ' ' << n_edges() / 2 << std::endl;
    for (auto &node : nodes)
      for (auto &next : node.adjacent)
        if (next.other_id > node.id)
          o << node.id << ' ' << next.other_id << std::endl;
  }

  const NODE_DATA & operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA & operator[](size_t ind) { return nodes[ind].data; }
};

template<class NODE_DATA, class EDGE_DATA>
struct graph<NODE_DATA, EDGE_DATA>::NODE::EDGE_ENTRY
{
  int other_id;
  EDGE_DATA data;

  EDGE_ENTRY() {}
  EDGE_ENTRY(int other_id, const EDGE_DATA &data)
    : other_id(other_id), data(data)
  {}
  EDGE_ENTRY(int other_id, EDGE_DATA &&data)
    : other_id(other_id), data(std::move(data))
  {}

  bool operator<(const EDGE_ENTRY &other) const
  {
    return other_id < other.other_id;
  }

  bool operator<(int id) const
  {
    return other_id < id;
  }

  friend bool operator<(int id, const EDGE_ENTRY &self)
  {
    return id < self.other_id;
  }
};

template<class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template<class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

struct NODE_DATA_T
{
};
struct EDGE_DATA_T
{
};

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

graph_t read_graph(std::istream &i)
{
  graph_t g;

  int N, M;
  i >> N >> M;

  g.resize(N);
  for (int j = 0; j < M; j++)
  {
    int adj1, adj2;
    i >> adj1 >> adj2;
    g.add_edge(adj1 - 1, adj2 - 1, {});
    g.add_edge(adj2 - 1, adj1 - 1, {});
  }
  return g;
}

using polynom = std::vector<int>;

polynom chroma_poly(graph_t g)
{
  int u = -1, v;
  g.dfs(
    [&](graph_t::EDGE_PROXY from_parent)
    {
      if (u != -1)
        return true;
      if (from_parent.from.is_valid())
      {
        // Found edge
        u = from_parent.from.id;
        v = from_parent.to.id;
        return true;
      }
      return false;
    },
    [](auto, auto){}, []{}, []{});
  if (u == -1)
  {
    // No edges
    polynom result(g.size() + 1);
    result[g.size()] = 1;
    return result;
  }
  // Edge exists
  polynom
    chroma_compressed = chroma_poly(g.compress_edge(u, v)),
    crhoma_removed = chroma_poly(g.remove_edge(u, v).remove_edge(v, u));
  chroma_compressed.push_back(0);
  std::transform(crhoma_removed.begin(), crhoma_removed.end(),
    chroma_compressed.begin(), crhoma_removed.begin(), std::minus<>());
  return crhoma_removed;
}

polynom multiply(polynom left, polynom right)
{
  polynom res(left.size() + right.size() - 1);
  for (size_t pow_left = 0; pow_left < left.size(); pow_left++)
    for (size_t pow_right = 0; pow_right < right.size(); pow_right++)
      res[pow_left + pow_right] += left[pow_left] * right[pow_right];
  return res;
}


int main()
{
  graph_t g = read_graph(std::cin);
  polynom poly = chroma_poly(g);
  while (!poly.empty() && poly.back() == 0)
    poly.pop_back();

  std::cout << poly.size() - 1 << std::endl;
  for (auto it = poly.rbegin(); it != poly.rend(); it++)
    std::cout << *it << ' ';
  std::cout << std::endl;

  return 0;
}
