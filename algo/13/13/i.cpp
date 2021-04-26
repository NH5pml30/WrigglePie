/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <cassert>
#include <stack>
#include <optional>
#include <variant>

namespace {
  template<typename T>
    constexpr T infty_v = std::numeric_limits<T>::max();

  constexpr int infty = infty_v<int>;
}

struct suffix_tree {
  struct node {
    int id, depth, start;
    node *parent;

    struct transition_table {
      std::array<node *, 'z' - 'a' + 2> data{};

      node *&operator[](size_t index) {
        assert(index == 0 || (index >= 'a' && index <= 'z'));
        return index == 0 ? data[0] : data[index - 'a' + 1];
      }

      node *operator[](size_t index) const {
        assert(index == 0 || (index >= 'a' && index <= 'z'));
        return index == 0 ? data[0] : data[index - 'a' + 1];
      }

      char from_iterator(decltype(data)::const_iterator iter) const {
        ptrdiff_t diff = iter - data.begin();
        return diff == 0 ? 0 : (char)('a' + (diff - 1));
      }
    } children = {};
    node *suffix_link = nullptr;

    node(int id, int depth, int start, node *parent, node *suffix_link = nullptr)
        : id(id), depth(depth), start(start), parent(parent), suffix_link(suffix_link) {
    }
  };
  struct state_t {
    suffix_tree *enclosing;
    node *cur_node, *last_added = nullptr;
    int L, R;

    state_t(suffix_tree *enclosing, node *cur_node, int L, int R)
        : enclosing(enclosing), cur_node(cur_node), L(L), R(R) {}

    int edge_offset() const {
      return R - L - cur_node->parent->depth;
    }
    char get_char(const char *sv) {
      return sv[cur_node->start + edge_offset()];
    }

    bool go_down(const char *sv) {
      char ch = sv[R];
      if (cur_node->depth != R - L) {
        int edge_offset_ = edge_offset();
        char edge_ch = get_char(sv);
        if (ch != edge_ch) {
          node *new_child = enclosing->alloc_node(cur_node->parent, R - L, cur_node->start);
          cur_node->parent->children[sv[cur_node->start]] = new_child;
          cur_node->parent = new_child;
          cur_node->start += edge_offset_;
          new_child->children[edge_ch] = cur_node;
          cur_node = new_child;
        }
      }
      if (cur_node->depth == R - L) {
        if (cur_node->children[ch] == nullptr) {
          node *new_child = enclosing->alloc_node(cur_node, infty, R);
          cur_node->children[ch] = new_child;
          cur_node = new_child;
          enclosing->iter_data.n_leafs++;
          enclosing->iter_data.n_substrs++;
          return true;
        } else {
          cur_node = cur_node->children[ch];
        }
      }
      return false;
    }

    node *next(const char *sv) {
      cur_node = cur_node->parent;
      node *res = nullptr;
      if (cur_node->suffix_link == nullptr) {
        res = cur_node;
        cur_node = cur_node->parent;
      }
      cur_node = cur_node->suffix_link;
      L++;
      while (cur_node->depth < R - L)
        cur_node = cur_node->children[sv[L + cur_node->depth]];
      return res;
    }

    void next_char(const char *sv) {
      enclosing->iter_data.n_substrs += enclosing->iter_data.n_leafs;
      while (L <= R) {
        bool created = go_down(sv);
        if (last_added != nullptr) {
          last_added->suffix_link = cur_node->parent;
          last_added = nullptr;
        }
        if (!created)
          break;

        last_added = next(sv);
      }
      R++;
    }
  };

  std::deque<node> nodes = {node(0, 0, -1, nullptr)};
  struct iterative_data {
    state_t state;
    uint64_t n_leafs = 0, n_substrs = 0;

    iterative_data(suffix_tree *enclosing, node *cur_node) : state(enclosing, cur_node, 0, 0) {}
  } iter_data = iterative_data(this, &nodes[0]);
  std::string s;

  std::optional<iterative_data> saved;

  suffix_tree() {
    nodes[0].parent = &nodes[0];
    nodes[0].suffix_link = &nodes[0];
  }

  suffix_tree(const suffix_tree &) = delete;
  suffix_tree(suffix_tree &&) = delete;
  suffix_tree &operator=(const suffix_tree &) = delete;
  suffix_tree &operator=(suffix_tree &&) = delete;

  node *alloc_node(node *parent, int depth, int start) {
    nodes.push_back(node{(int)nodes.size(), depth, start, parent});
    return &nodes.back();
  }

  void add_char(char ch) {
    s.push_back(ch);
    iter_data.state.next_char(s.data());
  }

  void add_string(const std::string &str) {
    s += str;
    for (int i = 0; i < (int)str.size(); i++)
      iter_data.state.next_char(s.data());
  }

  void revert() {
    iter_data = *saved;
  }

  node *operator[](int index) {
    return &nodes[index];
  }

  const node *operator[](int index) const {
    return &nodes[index];
  }
};

bool find_substr(const suffix_tree &t, std::string_view sv) {
  if (sv.size() > t.s.size())
    return false;

  const suffix_tree::node *cur_node = &t.nodes[0];
  while (!sv.empty()) {
    if (suffix_tree::node *child = cur_node->children[sv[0]]; child != nullptr) {
      size_t begin = child->start;
      size_t len = std::min<size_t>(child->depth - cur_node->depth, sv.size());
      if (sv.substr(0, len) != std::string_view(t.s.data() + begin, len)) {
        return false;
      }
      sv.remove_prefix(len);
      cur_node = child;
    } else {
      return false;
    }
  }
  return true;
}

template<class Action>
void walk_edges(const suffix_tree &t, Action &&action) {
  for (auto &v : t.nodes)
    for (char ch = 'a'; ch <= 'z'; ch++)
      if (int u = v.children[ch]; u != -1)
        action(v.id, u, t[u]->start, t[u]->depth == infty ? infty : t[u]->start + t[u]->depth - v.depth);
}

#include <sstream>

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  suffix_tree t;

#ifdef _DEBUG
  auto &&in = std::istringstream(R"_delim(? love
? is
A Loveis
? love
? WHO
A Whoareyou
? is
)_delim");
#else
  auto &&in = std::cin;
#endif
  auto &out = std::cout;

  char command;
  std::string buffer, str;
  while (in >> command) {
    str.clear();
    in >> str;
    std::transform(str.begin(), str.end(), str.begin(),
                   [](char ch) { return (char)std::tolower(ch); });

    if (command == '?') {
      t.add_string(buffer);
      buffer.clear();
      out << (find_substr(t, str) ? "YES" : "NO") << '\n';
    } else {
      buffer += str;
    }
  }

  out.flush();
  return 0;
}
