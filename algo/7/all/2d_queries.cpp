/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <cassert>
#include <functional>
#include <algorithm>

class tree_2d {
 private:
  struct frational_cascade {
    unsigned val;
    frational_cascade *left_src, *right_src;

    frational_cascade(unsigned val, frational_cascade *left_src = nullptr,
                      frational_cascade *right_src = nullptr)
        : val(val), left_src(left_src), right_src(right_src) {}

    frational_cascade() {}

    bool operator<(const frational_cascade &other) const {
      return val < other.val;
    }
  };

  std::vector<frational_cascade> ys;
  int n, logn;

  unsigned log2(unsigned x) {
    unsigned res = 0;
    for (; x > 0; x >>= 1, res++) {
    }
    return res - 1;
  }

  void merge(int level, int begin, int node_len) {
    frational_cascade
      *res = ys.data() + (level - 1) * n + begin,
      *left = res + n, *left_end = left + node_len,
      *right = left_end, *right_end = right + node_len;

    while (left != left_end || right != right_end) {
      if (right != right_end && left != left_end && left->val == right->val) {
        frational_cascade *left_begin = left, *right_begin = right;
        unsigned val = left->val;
        for (; left != left_end && left->val == val; left++)
          *res++ = {val, left_begin, right_begin};
        for (; right != right_end && right->val == val; right++)
          *res++ = {val, left_begin, right_begin};
        continue;
      }

      if (right == right_end || (left != left_end && left->val < right->val)) {
        *res++ = {left->val, left, right};
        left++;
      } else {
        *res++ = {right->val, left, right};
        right++;
      }
    }
  }

  unsigned count(int l, int r, int lx, int rx,
            frational_cascade *l_src, frational_cascade *r_src, frational_cascade *end) {
    if (l >= rx || lx >= r) return 0;
    if (lx >= l && rx <= r) return (unsigned)(r_src - l_src);
    int m = (lx + rx) / 2;
    frational_cascade *lend = end + n - (rx - lx) / 2, *rend = end + n;
    return count(l, r, lx, m,
                 l_src == end ? lend : l_src->left_src,
                 r_src == end ? lend : r_src->left_src, lend) +
           count(l, r, m, rx,
                 l_src == end ? rend : l_src->right_src,
                 r_src == end ? rend : r_src->right_src, rend);
  }

 public:
  tree_2d(const std::vector<unsigned> &y_sorted_by_x) {
    n = (int)y_sorted_by_x.size();
    assert((n & (n - 1)) == 0);  // power of 2
    logn = log2(n);
    int size = (logn + 1) * n;
    ys.resize(size);
    for (int i = 0; i < n; i++)
      ys[size - n + i] = {y_sorted_by_x[i], nullptr, nullptr};
    int node_len = 1;
    for (int level = logn; level > 0; level--, node_len *= 2) {
      for (int begin = 0; begin < n; begin += 2 * node_len)
        merge(level, begin, node_len);
    }
  }

  unsigned count(unsigned x1, unsigned x2, unsigned y1, unsigned y2) {
    size_t
      l = std::lower_bound(ys.begin(), ys.begin() + n, frational_cascade(y1)) - ys.begin(),
      r = std::lower_bound(ys.begin(), ys.begin() + n, frational_cascade(y2)) - ys.begin();
    return count(x1, x2, 0, n, ys.data() + l, ys.data() + r, ys.data() + n);
  }
};

class random_generator {
 private:
  unsigned a, b;
  unsigned cur;

 public:
  random_generator(unsigned a, unsigned b) : a(a), b(b), cur(0) {
  }

  void add_b(unsigned new_b) { b += new_b; }

  unsigned next_rand_17() {
    cur = cur * a + b;
    return cur >> 15;
  }

  unsigned next_rand_24() {
    cur = cur * a + b;
    return cur >> 8;
  }
};

int main() {
  int q;
  unsigned a, b;
  std::cin >> q >> a >> b;

  random_generator gen(a, b);

  std::vector<unsigned> f(1 << 17);
  for (size_t i = 0; i < 1 << 17; i++)
    f[i] = gen.next_rand_24();

  tree_2d tree(f);

  unsigned res = 0;
  for (int i = 0; i < q; i++) {
    unsigned l = gen.next_rand_17(), r = gen.next_rand_17();
    if (l > r) std::swap(l, r);
    unsigned x = gen.next_rand_24(), y = gen.next_rand_24();
    if (x > y) std::swap(x, y);

    unsigned c = tree.count(l, r + 1, x, y + 1);
    gen.add_b(c);
    res += c;
  }

  std::cout << res << std::endl;
}
