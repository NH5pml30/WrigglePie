/* Kholiavin Nikolai, M3138 */

#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

template<class type>
  type & minimum(type &a1, type &a2) {
    return a1 < a2 ? a1 : a2;
  }

template<class type>
  type & minimum(type &a1, type &a2, type &a3) {
    if (a1 < a2 && a1 < a3)
      return a1;
    if (a2 < a3)
      return a2;
    return a3;
  }

template<class type>
  type & minimum(type &a1, type &a2, type &a3, type &a4) {
    return minimum(minimum(a1, a2, a3), a4);
  }

template<class type>
  type & minimum(type &a1, type &a2, type &a3, type &a4, type &a5) {
    return minimum(minimum(a1, a2, a3), a4, a5);
  }

template<class type>
  type & median(type &a1, type &a2, type &a3, type &a4, type &a5) {
    std::swap(minimum(a1, a2, a3, a4, a5), a1);
    std::swap(minimum(a2, a3, a4, a5), a2);
    return minimum(a3, a4, a5);
  }

template<class type>
  type & median(type &a1, type &a2, type &a3, type &a4) {
    std::swap(minimum(a1, a2, a3, a4), a1);
    std::swap(minimum(a2, a3, a4), a2);
    return std::min(a3, a4);
  }

template<class type>
  type & median(type &a1, type &a2, type &a3) {
    std::swap(minimum(a1, a2, a3), a1);
    return std::min(a2, a3);
  }

template<class type>
  int kth(int L, int R, std::vector<type> &a, int k);

template<class type>
  int partition(int L, int R, std::vector<type> &a) {
    if (R - L == 1) {
      return L;
    }

    type x = a[(R + L) / 2];

    int b = L - 1, e = R;
    while (true) {
      do {
        b++;
      } while (a[b] < x);

      do {
        e--;
      } while (x < a[e]);

      if (b >= e)
        break;
      std::swap(a[b], a[e]);
    }
    return e;
  }

template<class type>
  int kth(int L, int R, std::vector<type> &a, int k) {
    if (R - L == 1)
      return L;

    int i = partition(L, R, a);
    if (k - 1 < i - L)
      return kth(L, i, a, k);
    else if (k - 1 == i - L)
      return i;
    else
      return kth(i + 1, R, a, k - (i - L + 1));
  }

class cashreg {
 public:
  static long long M;
  std::pair<int, int> *Data = nullptr;

  cashreg() {
  }

  cashreg(std::pair<int, int> *Data) : Data(Data) {
  }

  long long Val() const {
    if (Data->first == 0) {
      if (M >= Data->second)
        return LLONG_MAX;
      else
        return 0;
    }
    return std::max((M - Data->second) / Data->first, 0ll);
  }

  bool operator<(const cashreg &Other) const {
    return Val() < Other.Val();
  }
};

long long cashreg::M;

#include <fstream>

int main() {
  int m;
  std::cin >> m;
  std::vector<std::pair<int, int>> data(m);
  std::vector<cashreg> regs(m);

  for (int i = 0; i < m; i++) {
    int a, b, t;
    std::cin >> a >> b >> t;
    data[i] = {a, b + t};
    regs[i] = cashreg(&data[i]);
  }

  int n, p;
  std::cin >> n >> p;
  n = std::min(n, m);

  long long L = 0, R = 100'000 * ((long long)p * n + 2);

  while (L < R) {
    long long mid = (L + R) / 2;
    cashreg::M = mid;
    kth(0, m, regs, m - n + 1);

    long long max_bought = 0;
    for (int i = m - n; i < m; i++) {
      long long val = regs[i].Val();
      if (max_bought > LLONG_MAX - val) {
        max_bought = LLONG_MAX;
        break;
      }
      max_bought += val;
    }

    if (max_bought < p)
      L = mid + 1;
    else
      R = mid;
  }

  std::cout << R << std::endl;

  return 0;
}
