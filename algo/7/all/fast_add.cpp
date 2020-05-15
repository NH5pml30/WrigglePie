/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>

class random_generator {
 private:
  unsigned a, b;
  unsigned cur = 0;

 public:
  random_generator(unsigned a, unsigned b) : a(a), b(b) {}

  unsigned next_rand() {
    cur = cur * a + b;
    return cur >> 8;
  }
};

int main() {
  int m, q;
  std::cin >> m >> q;
  unsigned a, b;
  std::cin >> a >> b;
  random_generator gen(a, b);
  constexpr int n = 1 << 24;

  std::vector<unsigned> d(n);

  for (int i = 0; i < m; i++) {
    unsigned add = gen.next_rand();
    unsigned l = gen.next_rand(), r = gen.next_rand();
    if (l > r) std::swap(l, r);
    d[l] += add;
    if (r + 1 < n)
      d[r + 1] -= add;
  }

  unsigned last = 0;
  for (int i = 0; i < n; i++) {
    last += d[i];
    d[i] = (i == 0 ? 0 : d[i - 1]) + last;
  }

  unsigned result = 0;
  for (int i = 0; i < q; i++) {
    unsigned l = gen.next_rand(), r = gen.next_rand();
    if (l > r) std::swap(l, r);
    result += d[r] - (l == 0 ? 0 : d[l - 1]);
  }

  std::cout << result << std::endl;
  return 0;
}
