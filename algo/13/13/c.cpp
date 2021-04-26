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

namespace {
  int infty = std::numeric_limits<int>::max();
}

struct suffix_tree {
  struct node {
    int id, depth, start;
    node *parent;
    struct transition_table {
      std::array<node *, 'z' - 'a' + 2> data = {};

      node *&operator[](size_t index) {
        assert(index == 0 || (index >= 'a' && index <= 'z'));
        return index == 0 ? data[0] : data[index - 'a' + 1];
      }

      node *operator[](size_t index) const {
        assert(index == 0 || (index >= 'a' && index <= 'z'));
        return index == 0 ? data[0] : data[index - 'a' + 1];
      }
    };
    transition_table children = {};
    node *suffix_link = nullptr;
  };

  std::deque<node> nodes;

  struct state_t {
    node *cur_node;
    int L, R;

    int edge_offset(int R = -1) const {
      if (R == -1)
        R = this->R;
      return R - L - cur_node->parent->depth;
    }
    node *parent() const {
      return cur_node->parent;
    }
    char get_char(const char *sv, int R = -1) {
      return sv[cur_node->start + edge_offset(R)];
    }

    template<class NodeFactory>
    bool go_down(const char *sv, NodeFactory &&node_factory, int R = -1) {
      if (R == -1)
        R = this->R;

      char ch = sv[R];
      if (cur_node->depth != R - L) {
        int edge_offset_ = edge_offset(R);
        char edge_ch = get_char(sv, R);
        if (ch != edge_ch) {
          node *new_child = node_factory(cur_node->parent, R - L, cur_node->start);
          cur_node->parent->children[sv[cur_node->start]] = new_child;
          cur_node->parent = new_child;
          cur_node->start += edge_offset_;
          new_child->children[edge_ch] = cur_node;
          cur_node = new_child;
        }
      }
      if (cur_node->depth == R - L) {
        if (cur_node->children[ch] == nullptr) {
          node *new_child = node_factory(cur_node, infty, R);
          cur_node->children[ch] = new_child;
          cur_node = new_child;
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
  };

  suffix_tree(const std::string &s) {
    auto alloc_node = [&](node *parent, int depth, int start) {
      nodes.push_back(node{(int)nodes.size(), depth, start, parent});
      return &nodes.back();
    };
    alloc_node(nullptr, 0, -1);
    nodes[0].suffix_link = &nodes[0];

    state_t state = {&nodes.front(), 0, 0};

    node *last_added = nullptr;

    while (state.R < (int)s.length()) {
      while (state.L <= state.R) {
        bool created = state.go_down(s.c_str(), alloc_node, state.R);
        if (last_added != nullptr) {
          last_added->suffix_link = state.cur_node->parent;
          last_added = nullptr;
        }
        if (!created)
          break;

        last_added = state.next(s.c_str());
      }
      state.R++;
    }
  }
};

template<class Action>
void walk_edges(const suffix_tree &t, Action &&action) {
  for (auto &v : t.nodes)
    for (char ch = 'a'; ch <= 'z'; ch++)
      if (auto *u = v.children[ch]; u != nullptr)
        action(v.id, u->id, u->start, u->depth == infty ? infty : u->start + u->depth - v.depth);
}

int main() {
  std::string s;
  std::cin >> s;

  suffix_tree t(s);
  std::cout << t.nodes.size() << ' ' << t.nodes.size() - 1 << '\n';
  walk_edges(t, [len = (int)s.length()](int p, int c, int l, int r) {
    std::cout << p + 1 << ' ' << c + 1 << ' ' << l + 1 << ' ' << std::min(len, r) << '\n';
  });
  std::cout.flush();
  return 0;
}
