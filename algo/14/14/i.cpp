/* Nikolai Kholiavin, M3238 */
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <functional>
#include <iosfwd>
#include <iostream>
#include <numeric>
#include <random>
#include <sstream>
#include <stdexcept>
#include <tuple>
#include <vector>

template<bool invert = false, class group_info>
std::vector<typename group_info::type> FFT_helper(const std::vector<typename group_info::type> &a, const group_info &info) {
  int n = (int)a.size();

  if (n == 1) {
    return a;
  }

  std::vector<typename group_info::type> a0, a1;
  a0.reserve(n / 2), a1.reserve(n / 2);
  for (int i = 0; i < n / 2; i++) {
    a0.push_back(a[2 * i]);
    a1.push_back(a[2 * i + 1]);
  }
  auto y0 = FFT_helper<invert>(a0, info), y1 = FFT_helper<invert>(a1, info), y = std::vector<typename group_info::type>(n);
  for (int j = 0; j < n; j++)
    y[j] = info.add(y0[j % (n / 2)], info.mul(info.root(n, invert ? -j : j), y1[j % (n / 2)]));
  return y;
}

template<class group_info>
std::vector<typename group_info::type> FFT(const std::vector<typename group_info::type> &a,
                                           const group_info &info) {
  return FFT_helper(a, info);
}

template<class group_info>
std::vector<typename group_info::type> RFFT(const std::vector<typename group_info::type> &y,
                                            const group_info &info) {
  auto a = FFT_helper<true>(y, info);
  std::transform(a.begin(), a.end(), a.begin(),
                 [&, inv_n = info.inv((typename group_info::type)a.size())](auto el) {
                   return info.mul(inv_n, el);
                 });
  return a;
}

uint32_t round_up(uint32_t v) {
  v--;
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  v++;
  return v;
}

std::tuple<int, int, int> euclid_(int a, int b) {
  if (b == 0)
    return {a, 1, 0};
  else {
    auto [d, x_, y_] = euclid_(b, a % b);
    return {d, y_, x_ - a / b * y_};
  }
}

std::tuple<int, int, int> euclid(int a, int b) {
  if (a < b) {
    auto [d, x, y] = euclid_(b, a);
    return {d, y, x};
  } else {
    return euclid_(a, b);
  }
}

namespace modular {

template<typename type>
type mod(type a, type mod) {
  return (a % mod + mod) % mod;
}

int inv(int n, int mod) {
  auto [d, x, y] = euclid(n, mod);
  (void)y;
  assert(d == 1);
  return modular::mod(x, mod);
}

template<typename type>
int last_bit(type x) {
  for (int i = sizeof(x) * CHAR_BIT - 1; i >= 0; i--)
    if (((x >> i) & 1) != 0)
      return i;
  return -1;
}

int64_t mul(int64_t a, int64_t b, int64_t mod) {
  int64_t res = 0;
  for (int i = last_bit(b); i >= 0; i--) {
    res = (res * 2) % mod;
    if (((b >> i) & 1) != 0)
      res = (res + a) % mod;
  }
  return res;
}

int64_t pow(int64_t b, int64_t p, int64_t mod) {
  int64_t res = 1;
  for (int i = last_bit(p); i >= 0; i--) {
    res = mul(res, res, mod);
    if (((p >> i) & 1) != 0)
      res = mul(b, res, mod);
  }
  return res;
}

}  // namespace modular

struct bit_group_info {
  using type = int32_t;
  static constexpr type mod = 786'433;  // 3 * 2^18 + 1
  static constexpr type g = 10;
  std::vector<type> nroots;

  static int log2(int n) {
    int res = 0;
    while (n > 1) {
      n >>= 1;
      res++;
    }
    return res;
  }

  bit_group_info(int n_) : nroots(size_t{1} << log2(round_up(n_))) {
    nroots[0] = 1;
    type root = (type)modular::pow(g, (mod - 1) / nroots.size(), mod), cur = root;
    for (int i = 1; i < (int)nroots.size(); i++, cur = (type)modular::mul(cur, root, mod))
      nroots[i] = cur;
  }

  type root(int n, int j) const {
    return nroots[nroots.size() / n * ((j + n) % n)];
  }

  type add(type l, type r) const {
    return (l + r) % mod;
  }

  type mul(type l, type r) const {
    // return (type)modular::mul(l, r, mod);
    return (type)((int64_t)l * r % mod);
  }

  type inv(type a) const {
    return modular::inv(a, mod);
  }
};

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int N, H;
  std::cin >> N >> H;

  int n_pow2 = std::max(round_up(N + 1), 1u << (H + 1));
  bit_group_info info(n_pow2);

  std::vector<bit_group_info::type> gen_f_h_cur = {0, 1}, gen_f_h_last = {1},
                                    unity = {0, 1};  // h=0 and h=-1 (empty)
  gen_f_h_cur.resize(n_pow2);
  gen_f_h_last.resize(n_pow2);
  unity.resize(n_pow2);
  if (H == 0) {
    std::cout << gen_f_h_cur[N] << std::endl;
    return 0;
  }
  gen_f_h_cur = FFT(gen_f_h_cur, info);
  gen_f_h_last = FFT(gen_f_h_last, info);
  unity = FFT(unity, info);

  auto gen_f_h_next = std::vector<bit_group_info::type>(n_pow2);
  for (int i = 1; i <= H; i++) {
    using namespace std::placeholders;
    for (int i = 0; i < n_pow2; i++)
      gen_f_h_next[i] = info.mul(unity[i], info.mul(gen_f_h_cur[i], info.add(gen_f_h_cur[i], info.mul(2, gen_f_h_last[i]))));
    gen_f_h_last.swap(gen_f_h_cur);
    gen_f_h_cur.swap(gen_f_h_next);
  }

  auto ans = RFFT(gen_f_h_cur, info);
  std::cout << ans[N] << std::endl;
  return 0;
}
