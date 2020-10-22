/* Kholiavin Nikolai, M3138 */
#include <algorithm>
#include <cassert>
#include <functional>
#include <ios>
#include <iostream>
#include <list>
#include <numeric>
#include <set>
#include <stack>
#include <string>
#include <vector>

class disjoint_set {
 private:
  struct NODE_DATA {
    int index, parent, rank;
    int val = 0;

    NODE_DATA(int index) :
      index(index), parent(index), rank(index) {
    }

    NODE_DATA() {
    }

    static int attach(NODE_DATA &X, NODE_DATA &Y) {
      if (X.rank > Y.rank) {
        Y.parent = X.index;
        return X.index;
      } else {
        X.parent = Y.index;
        if (X.rank == Y.rank)
          Y.rank++;
        return Y.index;
      }
    }
  };

  std::vector<NODE_DATA> nodes;

  int find_update(int x) {
    if (nodes[x].parent == x)
      return x;
    return nodes[x].parent = find_update(nodes[x].parent);
  }

 public:
  disjoint_set(int n) : nodes(n) {
    int i = 0;
    for (auto &el : nodes)
      el = NODE_DATA(i++);
  }

  int find(int x) {
    return find_update(x);
  }

  int join(int x, int y) {
    x = find(x), y = find(y);
    if (x == y)
      return x;
    return NODE_DATA::attach(nodes[x], nodes[y]);
  }
};


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

 public:
  enum class color { white, gray, black };
  std::vector<color> cols;

 private:
  std::vector<NODE> nodes;
  size_t n_of_edges = 0;

 public:
  class NODE_PROXY {
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
          col(id >= 0 ? g.cols[id] : static_cast<const color &>(color())) {}
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

  void exec_dfs(
      const std::function<bool(EDGE_PROXY from_parent)> &before,
      const std::function<void(EDGE_PROXY from_parent,
                               const std::vector<EDGE_PROXY> &to_children)>
          &after,
      int start) {
    std::stack<std::tuple<EDGE_PROXY, std::vector<EDGE_PROXY>, bool>> st;
    st.push({EDGE_PROXY(*this, start), {}, true});
    while (!st.empty()) {
      auto[from_parent, to_children, in_first] = st.top();
      st.pop();
      int node_id = from_parent.to.id;
      if (in_first) {
        if (before(from_parent) || cols[node_id] != color::white) continue;
        cols[node_id] = color::gray;
        int node_id = from_parent.to.id;
        st.push({from_parent, to_children, false});
        std::vector<EDGE_PROXY> &to_children_ = std::get<1>(st.top());
        for (auto &u : nodes[node_id].adjacent) {
          EDGE_PROXY to_child = EDGE_PROXY(*this, node_id, u);
          st.push({to_child, {}, true});
          to_children_.push_back(to_child);
        }
      } else {
        after(from_parent, to_children);
        cols[from_parent.to.id] = color::black;
      }
    }
  }

  graph() {}

  void resize(size_t size) {
    size_t start = nodes.size();
    nodes.resize(size);
    cols.resize(size);
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

  void clear() {
    nodes.clear();
    cols.clear();
  }

  void add_edge(int from, int to, const EDGE_DATA &data = {}) {
    nodes[from].add_adjacent(to, data);
    n_of_edges++;
  }
  bool exists_edge(int from, int to) {
    return std::any_of(nodes[from].adjacent.begin(), nodes[from].adjacent.end(),
                       [&](const auto &x) { return x.other_id == to; });
  }

  void dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
           const std::function<void(EDGE_PROXY from_parent,
                                    const std::vector<EDGE_PROXY> &to_children)>
               &after,
           int start_at) {
    std::fill(cols.begin(), cols.end(), color::white);
    exec_dfs(before, after, start_at);
  }

  void dfs(const std::function<bool(EDGE_PROXY from_parent)> &before,
           const std::function<void(EDGE_PROXY from_parent,
                                    const std::vector<EDGE_PROXY> &to_children)>
               &after,
           const std::function<void()> &start_callback,
           const std::function<void()> &end_callback) {
    std::fill(cols.begin(), cols.end(), color::white);
    for (int i = 0; i < (int)nodes.size(); i++)
      if (cols[i] == color::white) {
        start_callback();
        exec_dfs(before, after, i);
        end_callback();
      }
  }

  const NODE_DATA &operator[](size_t ind) const { return nodes[ind].data; }
  NODE_DATA &operator[](size_t ind) { return nodes[ind].data; }
};

template <class NODE_DATA, class EDGE_DATA>
EDGE_DATA graph<NODE_DATA, EDGE_DATA>::EDGE_PROXY::null_data;
template <class NODE_DATA, class EDGE_DATA>
NODE_DATA graph<NODE_DATA, EDGE_DATA>::NODE_PROXY::null_data;

struct NODE_DATA_T {
  int
    depth,
    came_from,
    went_to;
  std::vector<int> shortcuts_down;
  int shortcut_up;
  bool backtrack_flag;
  std::list<int>::iterator place_in_list;
};
struct EDGE_DATA_T {
  int edge_id;
};

using graph_t = graph<NODE_DATA_T, EDGE_DATA_T>;

graph_t read_graph(std::istream &i) {
  graph_t g;

  int N, M;
  i >> N >> M;

  g.resize(N);
  for (int j = 0; j < M; j++) {
    int adj1, adj2;
    i >> adj1 >> adj2;
    g.add_edge(adj1 - 1, adj2 - 1, {});
    g.add_edge(adj2 - 1, adj1 - 1, {});
  }
  return g;
}

std::vector<int> find_cycle(graph_t &g) {
  disjoint_set ds(g.size());
  std::vector<int> ds_root2anc(g.size(), 0);

  enum class state {
    searching, finished
  } stt = state::searching;

  std::list<int> have_shortcut_down;

  int cycle_bot_left, cycle_bot_right, cycle_middle, cycle_top;
  // searching for loop cycle_top -> cycle_bot_right -> .. ->
  // -> cycle_bot_left -> cycle_middle -> .. -> cycle_top
  g.dfs(
      [&](graph_t::EDGE_PROXY from_parent) {
        if (stt != state::searching)
          return true;
        auto &vertex = from_parent.to.data;
        auto &parent = from_parent.from.data;
        if (from_parent.from.is_valid())
          parent.went_to = from_parent.to.id;
        switch (from_parent.to.col) {
          case graph_t::color::white:
            ds_root2anc[from_parent.to.id] = from_parent.to.id;
            vertex.depth = from_parent.from.is_valid() ? parent.depth + 1 : 0;
            vertex.came_from = from_parent.from.is_valid() ? from_parent.from.id : -1;
            vertex.shortcut_up = -1;
            vertex.backtrack_flag = false;
            vertex.place_in_list = have_shortcut_down.end();
            break;
          case graph_t::color::gray:
            if (int level_d = parent.depth - vertex.depth; level_d > 1) {
              if (level_d % 2 == 1) {
                // found loop with even length!
                stt = state::finished;
                cycle_bot_left = cycle_middle = cycle_top = from_parent.to.id;
                cycle_bot_right = from_parent.from.id;
              } else {
                // found shortcut to an even depth,
                // save the most shallow for processing later
                if (parent.shortcut_up == -1 ||
                  g[parent.shortcut_up].depth > vertex.depth)
                  parent.shortcut_up = from_parent.to.id;
                // save even depth shortcut for future generations
                if (vertex.shortcuts_down.empty()) {
                  have_shortcut_down.push_back(from_parent.to.id);
                  vertex.place_in_list = --have_shortcut_down.end();
                }
                vertex.shortcuts_down.push_back(from_parent.from.id);
              }
            }
            break;
          case graph_t::color::black:
          default:
            break;
        }
        return false;
      },
      [&](graph_t::EDGE_PROXY from_parent,
          const std::vector<graph_t::EDGE_PROXY> &) {
        if (stt == state::finished) {
          from_parent.to.data.backtrack_flag = stt == state::finished;
        } else {
          if (auto &vertex = from_parent.to.data; vertex.shortcut_up != -1) {
            for (int cur_id : have_shortcut_down)
              for (auto jump_to : g[cur_id].shortcuts_down)
                if (!(jump_to == from_parent.to.id && cur_id == vertex.shortcut_up)) {
                  if (int lca = ds_root2anc[ds.find(jump_to)];
                      g[lca].depth > g[vertex.shortcut_up].depth &&
                      g[lca].depth > g[cur_id].depth) {
                    // choose intersecting jump to complete the loop
                    cycle_top = vertex.shortcut_up;
                    cycle_middle = cur_id;
                    cycle_bot_left = jump_to;
                    cycle_bot_right = from_parent.to.id;
                    vertex.backtrack_flag = true;
                    stt = state::finished;
                    break;
                  }
                }
          }
          if (from_parent.from.is_valid()) {
            auto &parent = from_parent.from.data;
            ds_root2anc[ds.join(from_parent.to.id, from_parent.from.id)] =
                from_parent.from.id;
            parent.shortcuts_down.clear();
            if (parent.place_in_list != have_shortcut_down.end()) {
              have_shortcut_down.erase(parent.place_in_list);
              parent.place_in_list = have_shortcut_down.end();
            }
          }
        }
      },
      [&]() {},
      [&]() {});

  if (stt == state::finished) {
    std::vector<int> path;
    // go bot_left -> lca(bot_left, bot_right)
    int cur_id = cycle_bot_left;
    path.push_back(cur_id);
    while (!g[cur_id].backtrack_flag) {
      cur_id = g[cur_id].came_from;
      path.push_back(cur_id);
    }
    // go lca(bot_left, bot_right) -> bot_right
    while (cur_id != cycle_bot_right) {
      cur_id = g[cur_id].went_to;
      path.push_back(cur_id);
    }
    // go top -> middle
    int NODE_DATA_T::* go_to =
      g[cycle_top].depth < g[cycle_middle].depth ?
        &NODE_DATA_T::went_to : &NODE_DATA_T::came_from;
    cur_id = cycle_top;
    while (cur_id != cycle_middle) {
      path.push_back(cur_id);
      cur_id = g[cur_id].*go_to;
    }
    if (cycle_middle != cycle_bot_left)
      path.push_back(cycle_middle);
    return path;
  }
  return {};
}


int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int t;
  std::cin >> t;
  for (int i = 0; i < t; i++) {
    graph_t g = read_graph(std::cin);

    std::vector<int> path = find_cycle(g);
    if (!path.empty()) {
      std::cout << path.size() << std::endl;
      for (auto v : path)
        std::cout << v + 1 << ' ';
      std::cout << std::endl;
    } else { std::cout << "-1" << std::endl; }
  }
  return 0;
}
