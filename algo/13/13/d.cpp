/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <stack>

int32_t round_up_pow2(int32_t x) {
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x++;
  return x;
}

template<typename OutT>
std::vector<OutT> find_suffix_array(std::string str) {
  size_t saved_size = str.size();

  str.resize(round_up_pow2((int32_t)str.size() + 1), '$');
  size_t n = str.size();

  std::vector<OutT> c;
  std::transform(str.begin(), str.end(), std::back_inserter(c), [](char ch) { return (OutT)ch; });

  std::vector<OutT> h(std::max(n, (size_t)('z' + 1)), 0);
  for (auto clazz : c) h[clazz]++;
  std::transform(h.begin(), h.end(), h.begin(), [last = (OutT)0](OutT val) mutable {
    auto save = last;
    last = (OutT)(last + val);
    return save;
  });

  std::vector<OutT> p(str.size());
  {
    std::vector<OutT> h_copy = h;
    OutT i = 0;
    for (auto ch : str) p[h_copy[ch]++] = i++;
  }

  std::vector<OutT> p_prime(str.size());
  std::vector<std::pair<OutT, OutT>> t(str.size());
  for (size_t pow2 = 1; pow2 < n; pow2 *= 2) {
    t.clear();
    std::transform(c.begin(), c.end(), std::back_inserter(t), [&, i = 0](OutT clazz) mutable {
      return std::make_pair(clazz, c[(i++ + pow2) % n]);
    });

    {
      for (auto pi : p) {
        OutT sorted_lsd = (OutT)((pi - pow2 + n) % n);
        p_prime[h[c[sorted_lsd]]++] = sorted_lsd;
      }

      p.swap(p_prime);
    }

    std::pair<OutT, OutT> last = {(OutT)-1, (OutT)-1};
    auto clazz = (OutT)-1, i = (OutT)-1;

    for (auto pi : p) {
      ++i;
      if (t[pi] != last) {
        h[++clazz] = i;
        last = t[pi];
      }
      c[pi] = clazz;
    }
  }

  std::vector<OutT> res(saved_size);
  std::copy(p.begin() + n - saved_size, p.end(), res.begin());
  return res;
}

template<typename OutT>
std::vector<OutT> buildLCP(const std::string &str, const std::vector<OutT> &suf_arr) {
  std::vector<OutT> suf_inv(suf_arr.size());
  {
    OutT i = 0;
    for (auto si : suf_arr) suf_inv[si] = i++;
  }

  std::vector<OutT> lcp(str.size() - 1);
  OutT k = 0;
  for (int i = 0; i < (int)str.size(); i++) {
    OutT i_prime = suf_inv[i];
    if (i_prime == (int)(str.size() - 1)) {
      k = 0;
      continue;
    }

    OutT j = suf_arr[i_prime + 1];
    k = std::max<OutT>(0, (OutT)(k - 1));
    while (i + k < (int)str.size() && j + k < (int)str.size() && str[i + k] == str[j + k])
      k++;
    lcp[i_prime] = k;
  }

  return lcp;
}

int main() {
  std::string s;
  std::cin >> s;

  auto arr = find_suffix_array<int>(s);
  for (auto el : arr)
    std::cout << el + 1 << ' ';
  std::cout << '\n';

  auto lcp = buildLCP(s, arr);
  for (auto el : lcp)
    std::cout << el << ' ';
  std::cout << std::endl;
  return 0;
}
