/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <stack>
#include <algorithm>

class treap {
  struct NODE {
    int x, y, delta_x;
    NODE *left, *right;
    int min, max;
    bool is_continuous;

    void Update() {
      min = x;
      max = x;
      if (left != nullptr) {
        min = std::min(min, left->min + left->delta_x);
        max = std::max(max, left->max + left->delta_x);
      }
      if (right != nullptr) {
        min = std::min(min, right->min + right->delta_x);
        max = std::max(max, right->max + right->delta_x);
      }

      is_continuous =
        (left == nullptr ? true : left->is_continuous && x == left->max + left->delta_x + 1) &&
        (right == nullptr ? true : right->is_continuous && x == right->min + right->delta_x - 1);
    }

    NODE(int x, int y, NODE *left, NODE *right) :
      x(x), y(y), left(left), right(right) {
      Update();
    }

    static NODE * Merge(NODE *a, NODE *b) {
      NODE *res = nullptr;
      NODE **outer = &res;
      std::stack<NODE *> path;

      while (a != nullptr && b != nullptr) {
        if (a->y < b->y) {
          b->delta_x -= a->delta_x;
          *outer = a;
          path.push(a);
          outer = &(a->right);
          a = a->right;
        } else {
          a->delta_x -= b->delta_x;
          *outer = b;
          path.push(b);
          outer = &(b->left);
          b = b->left;
        }
      }
      *outer = (a == nullptr ? b : a);

      while (!path.empty()) {
        path.top()->Update();
        path.pop();
      }

      return res;
    }

    static std::pair<NODE *, NODE *> Split(NODE *at, int key) {
      struct LOCAL_DATA {
        NODE *t1, *t2;
        NODE *at;
        LOCAL_DATA *prev;
        int accumDelta;
        bool is_filled = false;

        LOCAL_DATA(NODE *at, LOCAL_DATA *prev) : t1(t1), t2(t2), at(at) {
          accumDelta =
            (prev == nullptr || prev->at == nullptr ?
              0 :
              prev->accumDelta + prev->at->delta_x);
        }
      };

      LOCAL_DATA root(nullptr, nullptr);

      std::stack<LOCAL_DATA> stack;
      stack.push(LOCAL_DATA(at, &root));

      while (!stack.empty()) {
        auto &data = stack.top();

        if (!data.is_filled) {
          LOCAL_DATA *saved = &data;
          if (data.at->x + data.at->delta_x + data.accumDelta <= key) {
            if (data.at->right == nullptr) {
              data.t1 = data.at;
              data.t2 = nullptr;
              data.is_filled = true;
            }
            else {
              stack.pop();
              stack.push(LOCAL_DATA(data.at->right, saved));
            }
          }
          else {
            if (data.at->left == nullptr) {
              data.t1 = nullptr;
              data.t2 = data.at;
              data.is_filled = true;
            }
            else {
              stack.pop();
              stack.push(LOCAL_DATA(data.at->left, saved));
            }
          }
        } else if (data.prev != nullptr) {
          if (data.at->x + data.at->delta_x + data.accumDelta <= key) {
            if (data.prev->at != nullptr) {
              data.prev->at->right = data.t1;
              data.prev->at->Update();
            }
            data.prev->t1 = data.prev->at;
            data.prev->t2 = data.t2;
          } else {
            if (data.prev->at != nullptr) {
              data.prev->at->left = data.t2;
              data.prev->at->Update();
            }
            data.prev->t1 = data.t1;
            data.prev->t2 = data.prev->at;
          }
          data.prev->is_filled = true;
        }
      }

      return {root.t1, root.t2};
    }

    void Increment(int delta) {
      delta_x++;
    }

    static std::pair<NODE *, NODE *> ContinuousCut(NODE *at) {
      std::stack<NODE *> stack;
      stack.push(at);
      while (!stack.empty()) {

      }
    }
  };

  NODE *root = nullptr;

  treap() {
  }


};


int main() {
  return 0;
}
