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

template<class group_info>
std::vector<typename group_info::type> mul(std::vector<typename group_info::type> &a,
                                           std::vector<typename group_info::type> &b,
                                           const group_info &info) {
  uint32_t size = round_up((uint32_t)(a.size() + b.size()));
  a.resize(size);
  b.resize(size);

  auto ya = FFT(a, info), yb = FFT(b, info);
  using namespace std::placeholders;
  std::transform(ya.begin(), ya.end(), yb.begin(), ya.begin(), std::bind(&group_info::mul, &info, _1, _2));
  return RFFT(ya, info);
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
  using type = uint32_t;
  static constexpr type mod = 6868'1729;  // 131 * 2^19 + 1
  static constexpr type g = 3;
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

  static std::vector<type> read(std::istream &i, int &sign) {
    std::vector<type> res;
    int ch;
    sign = 1;
    while ((ch = i.get()) != EOF)
      if (std::isdigit(ch)) {
        i.putback((char)ch);
        break;
      } else if (ch == '-') {
        sign = -1;
        break;
      }

    while ((ch = i.get()) != EOF) {
      if (!std::isdigit(ch)) {
        i.putback((char)ch);
        break;
      } else {
        res.push_back(ch - '0');
      }
    }

    std::reverse(res.begin(), res.end());
    return res;
  }

  static std::vector<type> normalize(std::vector<type> number) {
    // each 'digit' is <= 100 * |number|
    int carry = 0;
    for (auto &digit : number) {
      digit += carry;
      carry = digit / 10;
      digit %= 10;
    }
    while (carry > 0) {
      number.push_back(carry % 10);
      carry /= 10;
    }
    while (number.size() > 1 && number.back() == 0)
      number.pop_back();
    return number;
  }

  static void write(std::ostream &o, int sign, const std::vector<type> &number) {
    if (sign < 0 && number != std::vector<type>{0})
      o << '-';
    for (auto iter = number.rbegin(); iter != number.rend(); iter++)
      o << (int)*iter;
  }
};

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int signa = 1, signb = 1;
  auto a = bit_group_info::read(std::cin, signa);
  auto b = bit_group_info::read(std::cin, signb);

  bit_group_info::write(std::cout, signa * signb, bit_group_info::normalize(mul(a, b, bit_group_info((int)(a.size() + b.size())))));
  return 0;
}
