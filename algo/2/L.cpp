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

class gem {
 public:
  static double M;
  std::pair<int, int> *Data = nullptr;

  gem() {
  }

  gem(std::pair<int, int> *Data) : Data(Data) {
  }

  double Val() const {
    return Data->first - M * Data->second;
  }

  bool operator<(const gem &Other) const {
    return Val() < Other.Val();
  }
};

double gem::M;

int main() {
  int n, k;
  std::cin >> n >> k;
  std::vector<std::pair<int, int>> wv(n);
  std::vector<gem> gems(n);

  for (int i = 0; i < n; i++) {
    std::cin >> wv[i].first >> wv[i].second;
    gems[i] = gem(&wv[i]);
  }

  double L = 0, R = 1e7 + 1;

  for (int i = 0; i < 100; i++) {
    double m = (L + R) / 2;
    gem::M = m;

    kth(0, n, gems, n - k + 1);
    double val = 0;
    for (int i = n - k; i < n; i++) {
      val += gems[i].Val();
    }

    if (val < 0)
      R = m;
    else
      L = m;
  }

  for (int i = 0; i < k; i++)
    std::cout << gems[gems.size() - 1 - i].Data - wv.data() + 1 << " ";
  std::cout << std::endl;

  return 0;
}
