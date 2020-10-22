/* Kholiavin Nikolai, M3238 */
#include <algorithm>
#include <iostream>
#include <limits>
#include <queue>
#include <tuple>
#include <vector>

class binary_heap {
  using key_t = long long;

 public:
  struct DATA {
    key_t key;
    int id;

    DATA(key_t key, int id) : key(key), id(id) {}

    DATA() {}

    bool operator<(const DATA &other) const { return key < other.key; }

    bool operator>(const DATA &other) const { return key > other.key; }
  };

 private:
  std::vector<DATA> data;
  std::vector<int> idMap;

  void siftDown(int i) {
    for (int left = 2 * i + 1, right = 2 * i + 2;
         (left < (int)data.size() && data[i] > data[left]) ||
         (right < (int)data.size() && data[i] > data[right]);
         left = 2 * i + 1, right = 2 * i + 2) {
      int new_place;
      if (left >= (int)data.size())
        new_place = right;
      else if (right >= (int)data.size())
        new_place = left;
      else
        new_place = (data[left] < data[right] ? left : right);

      std::swap(idMap[data[i].id], idMap[data[new_place].id]);
      std::swap(data[i], data[new_place]);
      i = new_place;
    }
  }

  void siftUp(int i) {
    for (int parent = (i - 1) / 2; data[i] < data[parent];
         parent = (i - 1) / 2) {
      std::swap(idMap[data[i].id], idMap[data[parent].id]);
      std::swap(data[i], data[parent]);
      i = parent;
    }
  }

 public:
  binary_heap() {}

  void insert(key_t key, int id) {
    data.push_back(DATA(key, id));
    if (id >= (int)idMap.size()) idMap.resize(id + 1, -1);
    idMap[id] = data.size() - 1;
    siftUp(data.size() - 1);
  }

  DATA extractMin() {
    DATA res = data.front();
    idMap[res.id] = -1;
    data.front() = data.back();
    data.pop_back();

    if (data.size() > 0) {
      idMap[data.front().id] = 0;
      siftDown(0);
    }
    return res;
  }

  int size() { return data.size(); }

  bool decreaseKey(int id, key_t newKey) {
    if (id >= (int)idMap.size() || idMap[id] == -1) return false;
    data[idMap[id]].key = newKey;
    siftUp(idMap[id]);
    return true;
  }
};

template <typename T = int>
constexpr T infinity = std::numeric_limits<T>::max();

struct empty {};

template <class NODE_DATA = empty, class EDGE_DATA = empty>
class graph {
 private:
  struct NODE {
    struct EDGE_ENTRY {
      int other_id;
      EDGE_DATA data;

      EDGE_ENTRY() {}
      EDGE_ENTRY(int other_id, const EDGE_DATA &data)
          : other_id(other_id), data(data) {}
      EDGE_ENTRY(int other_id, EDGE_DATA &&data)
          : other_id(other_id), data(std::move(data)) {}

      bool operator<(const EDGE_ENTRY &other) const {
        return data < other.data;
      }
    };
    int id;
    NODE_DATA data;
    std::vector<EDGE_ENTRY> adjacent;

    NODE(int id = -1) : id(id) {}

    void add_adjacent(int other_id, const EDGE_DATA &data) {
      adjacent.push_back(EDGE_ENTRY(other_id, data));
    }
  };

 private:
  std::vector<NODE> nodes;
  size_t n_of_edges = 0;

 public:
  class NODE_PROXY {
    friend class EDGE_PROXY;

   public:
    const int id;
    NODE_DATA &data;

    // private:
    static NODE_DATA null_data;

    bool is_valid() const { return &data != &null_data; }

    NODE_PROXY(graph &g, int id)
        : id(id), data(id >= 0 ? g.nodes[id].data : null_data) {}
  };

  class EDGE_PROXY {
    friend class graph;

   public:
    const NODE_PROXY from, to;
    EDGE_DATA &edge_data;

    // private:
    static EDGE_DATA null_data;

    EDGE_PROXY(graph &g, int from, typename NODE::EDGE_ENTRY &entry)
        : from(g, from), to(g, entry.other_id), edge_data(entry.data) {}

    EDGE_PROXY(graph &g, int to)
        : from(g, -1), to(g, to), edge_data(null_data) {}
  };

  graph() {}

  void resize(size_t size) {
    size_t start = nodes.size();
    nodes.resize(size);
    for (size_t i = start; i < size; i++) nodes[i].id = (int)i;
  }

  graph reverse() {
    graph res;
    res.resize(size());
    for (auto &n : nodes)
      for (auto &e : n.adjacent) res.add_edge(e.other_id, n.id, e.data);
    return res;
  }

  size_t size() const { return nodes.size(); }
  size_t n_edges() const { return n_of_edges; }

  void clear() { nodes.clear(); }

  void add_edge(int from, int to, const EDGE_DATA &data = {}) {
    nodes[from].add_adjacent(to, data);
    n_of_edges++;
  }
  bool exists_edge(int from, int to) {
    return std::any_of(nodes[from].adjacent,
                       [&](const auto &x) { return x.first == to; });
  }

  std::enable_if_t<std::is_same_v<EDGE_DATA, int>, std::vector<long long>>
  dijkstra(int start) {
    std::vector<long long> dist(nodes.size(), infinity<long long>);
    dist[start] = 0;
    binary_heap prior_queue;
    prior_queue.insert(0, start);

    while (prior_queue.size() != 0) {
      int v = prior_queue.extractMin().id;
      // relax
      for (auto &next : nodes[v].adjacent)
        if (dist[v] + next.data < dist[next.other_id]) {
          dist[next.other_id] = dist[v] + next.data;
          if (!prior_queue.decreaseKey(next.other_id,
                                       dist[v] + (long long)next.data))
            prior_queue.insert(dist[v] + (long long)next.data, next.other_id);
        }
    }
    return dist;
  }

  const NODE_DATA &operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA &operator[](size_t ind) { return nodes[ind].data; }
};

template <class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template <class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

struct NODE_DATA_T {};
using EDGE_DATA_T = int;

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

graph_t read_graph(std::istream &in) {
  int n, m;
  in >> n >> m;

  graph_t g;
  g.resize(n);

  for (int i = 0; i < m; i++) {
    int from, to, w;
    in >> from >> to >> w;
    g.add_edge(from - 1, to - 1, w);
    g.add_edge(to - 1, from - 1, w);
  }
  return g;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  graph_t g = read_graph(std::cin);
  std::vector<long long> dist = g.dijkstra(0);
  for (int i = 0; i < (int)dist.size(); i++)
    std::cout << (dist[i] == infinity<long long> ? -1ll : dist[i]) << ' ';
  std::cout << std::endl;
  return 0;
}
