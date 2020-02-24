/* Kholiavin Nikolai, M3138 */
#include <algorithm>
#include <functional>
#include <iostream>
#include <stack>
#include <vector>

// #define NOMINMAX
// #include <windows.h>

#include <random>

class treap {
 public:
  struct NODE {
    static std::default_random_engine eng;
    int x, y, data, delta_x = 0;
    NODE *left, *right;
    int min, max;
    bool is_continuous;

    static void Update(NODE *at) {
      if (at == nullptr) return;

      at->min = at->x;
      at->max = at->x;
      if (at->left != nullptr) {
        at->min = std::min(at->min, at->left->min + at->left->delta_x);
        at->max = std::max(at->max, at->left->max + at->left->delta_x);
      }
      if (at->right != nullptr) {
        at->min = std::min(at->min, at->right->min + at->right->delta_x);
        at->max = std::max(at->max, at->right->max + at->right->delta_x);
      }

      at->is_continuous =
          (at->left == nullptr
               ? true
               : at->left->is_continuous &&
                     at->x == at->left->max + at->left->delta_x + 1) &&
          (at->right == nullptr
               ? true
               : at->right->is_continuous &&
                     at->x == at->right->min + at->right->delta_x - 1);
    }

    NODE(int x, int y, NODE *left, NODE *right)
        : x(x), y(eng()), data(y), left(left), right(right) {
      Update(this);
    }

    static NODE *Merge(NODE *a, NODE *b) {
      NODE *res = nullptr;
      NODE **outer = &res;
      std::stack<NODE *> path;

      while (a != nullptr && b != nullptr) {
        if (a->y < b->y) {
          b->delta_x -= a->delta_x;
          NODE *saved_a = a;
          *outer = a;
          path.push(saved_a);
          outer = &(saved_a->right);
          a = saved_a->right;
        } else {
          a->delta_x -= b->delta_x;
          NODE *saved_b = b;
          *outer = b;
          path.push(saved_b);
          outer = &(saved_b->left);
          b = saved_b->left;
        }
      }
      *outer = (a == nullptr ? b : a);

      while (!path.empty()) {
        Update(path.top());
        path.pop();
      }

      return res;
    }

    // 0  1 2 3 4  5
    //|  | [x] |  |
    //|  |/   \|  |
    //| [x]   [x] |
    //| / \   / \ |
    static std::pair<NODE *, NODE *> SplitF(
        NODE *at,
        const std::function<int(NODE *node, int accumDelta)> &where_to_split) {
      if (at == nullptr) return {nullptr, nullptr};

      struct LOCAL_DATA {
        NODE *t1 = nullptr, *t2 = nullptr;
        NODE *at;
        int accum_delta;
        int split_at = -1;
        bool is_filled = false;

        LOCAL_DATA *prev;

        LOCAL_DATA(NODE *at, LOCAL_DATA *prev) : at(at), prev(prev) {
          accum_delta = (prev == nullptr || prev->at == nullptr
                             ? 0
                             : prev->at->delta_x + prev->accum_delta);
        }
      };

      std::stack<LOCAL_DATA *> stack;
      std::vector<LOCAL_DATA *> alloc;
      LOCAL_DATA root = LOCAL_DATA(nullptr, nullptr);
      alloc.push_back(new LOCAL_DATA(at, &root));
      stack.push(alloc.back());

      while (!stack.empty()) {
        LOCAL_DATA *data = stack.top();

        if (!data->is_filled) {
          int split_at = where_to_split(data->at, data->accum_delta);
          data->split_at = split_at;
          switch (split_at) {
            case 0:
              stack.pop();
              data->prev->t1 = nullptr;
              data->prev->t2 = data->at;
              data->prev->is_filled = true;
              break;
            case 1:
              alloc.push_back(new LOCAL_DATA(data->at->left, data));
              stack.push(alloc.back());
              break;
            case 2:
              stack.pop();
              if (data->at->left != nullptr)
                data->at->left->delta_x += data->at->delta_x;
              data->prev->t1 = data->at->left;
              data->at->left = nullptr;
              data->prev->t2 = data->at;
              data->prev->is_filled = true;
              Update(data->prev->t1);
              Update(data->prev->t2);
              break;
            case 3:
              stack.pop();
              data->prev->t1 = data->at;
              if (data->at->right != nullptr)
                data->at->right->delta_x += data->at->delta_x;
              data->prev->t2 = data->at->right;
              data->at->right = nullptr;
              data->prev->is_filled = true;
              Update(data->prev->t1);
              Update(data->prev->t2);
              break;
            case 4:
              alloc.push_back(new LOCAL_DATA(data->at->right, data));
              stack.push(alloc.back());
              break;
            case 5:
              stack.pop();
              data->prev->t1 = data->at;
              data->prev->t2 = nullptr;
              data->prev->is_filled = true;
              break;
          }
        } else {
          int split_at = data->split_at;
          LOCAL_DATA *prev = data->prev;

          switch (split_at) {
            case 1:
              data->at->left = data->t2;
              if (data->t1 != nullptr) data->t1->delta_x += data->at->delta_x;
              prev->t1 = data->t1;
              prev->t2 = data->at;
              Update(prev->t1);
              Update(prev->t2);
              break;
            case 4:
              data->at->right = data->t1;
              if (data->t2 != nullptr) data->t2->delta_x += data->at->delta_x;
              prev->t1 = data->at;
              prev->t2 = data->t2;
              Update(prev->t1);
              Update(prev->t2);
              break;
            default:
              throw "fuckich";
          }
          prev->is_filled = true;
          stack.pop();
        }
      }

      for (auto data : alloc) delete data;
      return {root.t1, root.t2};
    }

    static std::pair<NODE *, NODE *> Split(NODE *at, int key) {
      return SplitF(at, [key](NODE *node, int accum_delta) {
        if (node->x + node->delta_x + accum_delta <= key) {
          if (node->right == nullptr) return 5;
          return 4;
        } else {
          if (node->left == nullptr) return 0;
          return 1;
        }
      });
    }

    static std::pair<NODE *, NODE *> SplitContinuous(NODE *at) {
      return SplitF(at, [](NODE *node, int) {
        if (node->left == nullptr || node->left->is_continuous) {
          if (node->left == nullptr ||
              node->x == node->left->max + node->left->delta_x + 1) {
            if (node->right == nullptr ||
                node->x == node->right->min + node->right->delta_x - 1) {
              if (node->right == nullptr || node->right->is_continuous)
                return 5;
              return 4;
            }
            return 3;
          }
          return 2;
        }
        return 1;
      });
    }

    static void Increment(NODE *at) {
      if (at != nullptr) at->delta_x++;
    }

    static bool Contains(NODE *at, int x) {
      int accum_delta = 0;
      while (at != nullptr) {
        int comp = at->x + at->delta_x + accum_delta - x;
        accum_delta += at->delta_x;
        if (comp < 0) {
          at = at->right;
        } else if (comp == 0) {
          return true;
        } else {
          at = at->left;
        }
      }
      return false;
    }
  };

 private:
  NODE *root = nullptr;
  std::vector<NODE *> nodes;

 public:
  treap() {}

  int GetMin() { return root->min + root->delta_x; }

  int GetMax() { return root->max + root->delta_x; }

  void Insert(int x, int y) {
    if (NODE::Contains(root, x)) {
      auto[l, r] = NODE::Split(root, x - 1);
      auto[rl, rr] = NODE::SplitContinuous(r);
      NODE::Increment(rl);
      root = NODE::Merge(NODE::Merge(l, rl), rr);
    }
    auto[l, r] = NODE::Split(root, x);
    nodes.push_back(new NODE(x, y, nullptr, nullptr));
    root = NODE::Merge(NODE::Merge(l, nodes.back()), r);
  }

  void Traverse(std::function<void(NODE *at, int accum_delta)> func) {
    std::stack<std::pair<NODE *, int>> path;
    path.push({root, 0});

    while (!path.empty()) {
      auto[cur, accum_delta] = path.top();
      func(cur, accum_delta);
      path.pop();
      if (cur->left != nullptr)
        path.push({cur->left, accum_delta + cur->delta_x});
      if (cur->right != nullptr)
        path.push({cur->right, accum_delta + cur->delta_x});
    }
  }

  /*
  void Print(int x, int y, int h) {
    int w = GetMax() + 1;
    std::wstring even_a = L"-+", odd_a = L" |", even, odd;
    for (int i = 0; i < w; i++) even += even_a, odd += odd_a;
    HANDLE console = GetStdHandle(STD_OUTPUT_HANDLE);
    for (int i = 0; i < h; i++) {
      SetConsoleCursorPosition(console, {(short)x, (short)(y + i * 2)});
      WriteConsole(console, odd.data(), odd.size(), NULL, NULL);
      SetConsoleCursorPosition(console, {(short)x, (short)(y + i * 2 + 1)});
      WriteConsole(console, even.data(), even.size(), NULL, NULL);
    }
    std::wstring buf = L"*";
    Traverse([=](NODE *at, int accum_delta) {
      SetConsoleCursorPosition(
          console, {(short)(x + (at->x + at->delta_x + accum_delta) * 2),
                    (short)(y + at->y * 2)});
      WriteConsole(console, buf.data(), 1, NULL, NULL);
    });
    SetConsoleCursorPosition(console, {(short)0, (short)y});
  }
  */

  ~treap() {
    for (auto n : nodes) delete n;
  }
};

std::default_random_engine treap::NODE::eng;

int main() {
  treap tree;

  int N, M;
  std::cin >> N >> M;
  for (int i = 0; i < N; i++) {
    int l;
    std::cin >> l;
    tree.Insert(l, i + 1);
    // tree.Print(30, 0, N);
  }

  std::vector<int> data(tree.GetMax());
  tree.Traverse([&data](treap::NODE *at, int accum_delta) {
    data[at->x + at->delta_x + accum_delta - 1] = at->data;
  });
  std::cout << tree.GetMax() << std::endl;
  for (int i = 0; i < tree.GetMax(); i++) {
    std::cout << data[i] << ' ';
  }
  std::cout << std::endl;
  return 0;
}
