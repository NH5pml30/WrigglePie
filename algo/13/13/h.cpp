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

    // state_t(const state_t &) = delete;
    // state_t(state_t &&) = delete;
    // state_t &operator=(const state_t &) = delete;
    // state_t &operator=(state_t &&) = delete;

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

    std::optional<char> try_go_down(const char *sv) {
      std::optional<char> res;
      if (cur_node->depth == R - L) {
        if (auto place =
                std::find_if(cur_node->children.data.begin(), cur_node->children.data.end(),
                             [](node *ptr) { return ptr != nullptr; });
            place != cur_node->children.data.end())
          res = cur_node->children.from_iterator(place);
        else {
          enclosing->iter_data.n_leafs++;
          enclosing->iter_data.n_substrs++;
        }
      } else {
        res = get_char(sv);
      }
      return res;
    }

    node *try_next(const char *sv) {
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

    node *next(const char *sv) {
      return try_next(sv);
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

    char try_next_char(const char *sv) {
      char result = 'a';
      enclosing->iter_data.n_substrs += enclosing->iter_data.n_leafs;
      while (L <= R) {
        std::optional<char> ch = try_go_down(sv);
        if (ch.has_value()) {
          result = *ch;
          break;
        }

        try_next(sv);
      }
      R++;
      return result;
    }
  };

  std::deque<node> nodes = {node(0, 0, -1, nullptr)};
  struct iterative_data {
    state_t state;
    uint64_t n_leafs = 0, n_substrs = 0;

    iterative_data(suffix_tree *enclosing, node *cur_node) : state(enclosing, cur_node, 0, 0) {}
  } iter_data = iterative_data(this, &nodes[0]);
  std::vector<char> s;

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

  char try_add_char() {
    saved = iter_data;
    return iter_data.state.try_next_char(s.data());
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

template<class Action>
void walk_edges(const suffix_tree &t, Action &&action) {
  for (auto &v : t.nodes)
    for (char ch = 'a'; ch <= 'z'; ch++)
      if (int u = v.children[ch]; u != -1)
        action(v.id, u, t[u]->start, t[u]->depth == infty ? infty : t[u]->start + t[u]->depth - v.depth);
}

int main() {
  int n;
  std::cin >> n;

  suffix_tree t;

  for (int i = 0; i < n; i++) {
    char c;
    std::cin >> c;

    if (c == '?') {
      char ch = t.try_add_char();
      std::cout << ch << ' ' << t.iter_data.n_substrs << '\n';
      t.revert();
      t.add_char(ch);
    } else {
      t.add_char(c);
    }
  }

  std::cout.flush();
  return 0;
}
