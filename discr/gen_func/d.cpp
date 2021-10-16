/* Nikolai Kholiavin, M3238 */
#include <type_traits>
#include <vector>
#include <deque>
#include <iostream>
#include <functional>
#include <algorithm>
#include <numeric>
#include <cassert>

// pre:
//   mod is prime
//   (mod - 1) * (mod - 1) fits into type
//   (mod - 1) + (mod - 1) fits into type
template<typename type, type mod>
struct modular_number
{
  static_assert(std::is_nothrow_default_constructible_v<type> &&
                std::is_nothrow_copy_constructible_v<type> &&
                std::is_nothrow_move_constructible_v<type> &&
                std::is_nothrow_copy_assignable_v<type> && std::is_nothrow_move_assignable_v<type>);

  type data;

  modular_number(type data) noexcept : data((data % mod + mod) % mod)
  {
  }
  modular_number() = default;

  friend modular_number operator+(modular_number lhs, modular_number rhs) noexcept
  {
    return (lhs.data + rhs.data) % mod;
  }
  modular_number &operator++() noexcept {
    *this += 1;
    return *this;
  }
  modular_number operator++(int) noexcept {
    auto save = modular_number(*this);
    ++*this;
    return save;
  }

  friend modular_number operator-(modular_number lhs, modular_number rhs) noexcept
  {
    auto res = (lhs.data - rhs.data) % mod;
    return res < 0 ? mod + res : res;
  }
  modular_number &operator--() noexcept {
    *this -= 1;
    return *this;
  }
  modular_number operator--(int) noexcept {
    auto save = modular_number(*this);
    --*this;
    return save;
  }

  friend modular_number operator*(modular_number lhs, modular_number rhs) noexcept
  {
    return (lhs.data * rhs.data) % mod;
  }

  friend modular_number pow(modular_number lhs, size_t rhs) noexcept
  {
    modular_number res = 1;
    for (modular_number powa = lhs; rhs > 0; powa *= powa, rhs >>= 1)
      if (rhs & 1) res *= powa;
    return res;
  }

  friend modular_number operator/(modular_number lhs, modular_number rhs) noexcept
  {
    return lhs * pow(rhs, mod - 2);
  }

  modular_number &operator+=(modular_number other) noexcept
  {
    return *this = *this + other;
  }
  modular_number &operator-=(modular_number other) noexcept
  {
    return *this = *this - other;
  }
  modular_number &operator*=(modular_number other) noexcept
  {
    return *this = *this * other;
  }

  bool operator==(modular_number other) const noexcept
  {
    return data == other.data;
  }
  bool operator!=(modular_number other) const noexcept
  {
    return !operator==(other);
  }
};

template<typename type, type mod>
std::istream &operator>>(std::istream &i, modular_number<type, mod> &n)
{
  type data;
  i >> data;
  n = modular_number<type, mod>(data);
  return i;
}

template<typename type, type mod>
std::ostream &operator<<(std::ostream &o, modular_number<type, mod> &n)
{
  return o << n.data;
}

using mod_int_t = modular_number<int64_t, 998'244'353>;

template<typename type>
using polynom = std::deque<type>;

template<typename type>
polynom<type> &normalize(polynom<type> &poly)
{
  while (!poly.empty() && poly.back() == type(0))
    poly.pop_back();
  return poly;
}

template<typename type>
polynom<type> operator+(const polynom<type> &lhs, const polynom<type> &rhs)
{
  polynom<type> res;
  const polynom<type> *min_size = &lhs, *max_size = &rhs;
  if (lhs.size() > rhs.size())
    std::swap(min_size, max_size);
  std::transform(min_size->begin(), min_size->end(), max_size->begin(), std::back_inserter(res), std::plus<>());
  std::copy(max_size->begin() + min_size->size(), max_size->end(), std::back_inserter(res));
  return normalize(res);
}

template<typename type>
polynom<type> &operator+=(polynom<type> &lhs, const polynom<type> &rhs)
{
  lhs.resize(std::max(lhs.size(), rhs.size()), type(0));
  std::transform(rhs.begin(), rhs.end(), lhs.begin(), lhs.begin(), std::plus<>());
  return normalize(lhs);
}

template<typename type>
std::ostream &operator<<(std::ostream &o, const polynom<type> &poly)
{
  for (auto el : poly)
    o << el << ' ';
  return o;
}

// computes sum(i=0,n)(lhs[i] * rhs[n - i] when i, n - i in range)
template<typename InItl, typename InItr>
auto convolve(InItl lhs_begin, InItl lhs_end,
              InItr rhs_begin, InItr rhs_end, size_t n_)
{
  using type = std::remove_const_t<typename InItl::value_type>;

  ptrdiff_t
    lhs_size = (ptrdiff_t)(lhs_end - lhs_begin),
    rhs_size = (ptrdiff_t)(rhs_end - rhs_begin),
    n = (ptrdiff_t)n_,
    lhs_offb = std::clamp(n - rhs_size, ptrdiff_t{0}, lhs_size),
    lhs_offe = std::clamp(n, lhs_offb, lhs_size),
    rhs_offb = std::clamp(rhs_size - (n - lhs_offb), ptrdiff_t{0}, rhs_size);
  return std::transform_reduce(
    lhs_begin + lhs_offb,
    lhs_begin + lhs_offe,
    std::make_reverse_iterator(rhs_end) + rhs_offb,
    type(0));
}

template<typename type>
polynom<type> operator*(const polynom<type> &lhs, const polynom<type> &rhs)
{
  polynom<type> res;
  std::generate_n(std::back_inserter(res), lhs.size() + rhs.size() - 1,
                  [&, i = (size_t)0]() mutable {
                    ++i;
                    return convolve(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), i);
                  });
  return res;
}

template<typename type>
polynom<type> &operator*=(polynom<type> &lhs, const polynom<type> &rhs)
{
  return lhs = lhs * rhs;
}

template<typename type>
polynom<type> operator*(type lhs, const polynom<type> &rhs)
{
  polynom<type> res;
  std::transform(rhs.begin(), rhs.end(), std::back_inserter(res),
                 [=](type el) { return el * lhs; });
  return res;
}
template<typename type>
polynom<type> operator*(const polynom<type> &lhs, type rhs)
{
  return operator*(rhs, lhs);
}
template<typename type>
polynom<type> &operator*=(polynom<type> &lhs, type rhs)
{
  std::transform(lhs.begin(), lhs.end(), lhs.begin(),
                 [=](type el) { return el * rhs; });
  return lhs;
}

template<typename type>
polynom<type> operator/(const polynom<type> &lhs, type rhs)
{
  polynom<type> res;
  std::transform(rhs.begin(), rhs.end(), std::back_inserter(res),
                 [=](type el) { return el / rhs; });
  return res;
}
template<typename type>
polynom<type> &operator/=(polynom<type> &lhs, type rhs)
{
  std::transform(lhs.begin(), lhs.end(), lhs.begin(),
                 [=](type el) { return el / rhs; });
  return lhs;
}

template<typename type>
void divide(const polynom<type> &lhs, const polynom<type> &rhs, polynom<type> &res)
{
  assert(rhs.size() > 0 && rhs[0] != 0);
  // auto lhs_begin = std::find_if(lhs.begin(), lhs.end(), [&](type val) { return val != 0; });
  // auto rhs_begin = std::find_if(rhs.begin(), rhs.end(), [&](type val) { return val != 0; });

  // assert(rhs_begin != rhs.end() && (lhs_begin - lhs.begin()) >= (rhs_begin - rhs.begin));

  std::generate(res.begin(), res.end(), [&, i = (size_t)0]() mutable {
    ++i;
    return (type(i > lhs.size() ? 0 : lhs[i - 1]) -
            convolve(res.begin(), res.end(), rhs.begin(), rhs.end(), i)) /
           rhs[0];
  });
}

template<typename type>
struct ratio
{
  static_assert(std::is_nothrow_default_constructible_v<type> &&
                std::is_nothrow_copy_constructible_v<type> &&
                std::is_nothrow_move_constructible_v<type> &&
                std::is_nothrow_copy_assignable_v<type> && std::is_nothrow_move_assignable_v<type>);

  type num, denom;

  void normalize() noexcept
  {
    if (denom < 0)
      num *= -1, denom *= -1;
    type gcd = std::gcd(num, denom);
    num /= gcd, denom /= gcd;
  }

  ratio(type num = type(0), type denom = type(1)) noexcept : num(num), denom(denom)
  {
    normalize();
  }

  friend ratio operator+(ratio lhs, ratio rhs) noexcept
  {
    type lcm = std::lcm(lhs.denom, rhs.denom);
    type fac1 = lcm / lhs.denom, fac2 = lcm / rhs.denom;
    return ratio(fac1 * lhs.num + fac2 * rhs.num, lcm);
  }
  ratio &operator+=(ratio rhs) noexcept
  {
    return *this = *this + rhs;
  }

  friend ratio operator-(ratio lhs, ratio rhs) noexcept
  {
    return lhs + ratio(rhs.num, rhs.denom);
  }
  ratio &operator-=(ratio rhs) noexcept
  {
    return *this = *this - rhs;
  }

  friend ratio operator*(ratio lhs, ratio rhs) noexcept
  {
    return ratio(lhs.num * rhs.num, lhs.denom * rhs.denom);
  }
  ratio &operator*=(ratio rhs) noexcept
  {
    return *this = *this * rhs;
  }

  friend ratio operator/(ratio lhs, ratio rhs) noexcept
  {
    return ratio(lhs.num * rhs.denom, lhs.denom * rhs.num);
  }
  ratio &operator/=(ratio rhs) noexcept
  {
    return *this = *this / rhs;
  }

  friend bool operator==(ratio lhs, ratio rhs) noexcept
  {
    return lhs.num == rhs.num && lhs.denom == rhs.denom;
  }
  friend bool operator!=(ratio lhs, ratio rhs) noexcept
  {
    return !operator==(lhs, rhs);
  }
};

template<typename type>
std::ostream &operator<<(std::ostream &o, ratio<type> rhs)
{
  return o << rhs.num << '/' << rhs.denom;
}

template<typename type>
polynom<type> compute_fn(type r, size_t k, const polynom<type> &p)
{

  polynom<type> pi, sum = {};
  auto copy = pi;

  type rpow = 1;
  for (size_t i = 0; i < p.size(); i++)
  {
    pi = {type(p[i]) / rpow};
    for (size_t j = 0; j < k; j++)
    {
      copy = pi;
      copy.push_front(0);
      copy *= (type(1) / type(j + 1));
      pi *= type((ptrdiff_t)(j - i + 1)) / type(j + 1);
      pi += copy;
    }
    sum += pi;
    rpow *= r;
  }

  return sum;
}

int main() {
  using poly_t = polynom<ratio<int64_t>>;

  int r, k;
  std::cin >> r >> k;

  poly_t p(k + 1);
  for (auto &el : p)
  {
    int num;
    std::cin >> num;
    el = num;
  }
  normalize(p);

  auto fn = compute_fn<ratio<int64_t>>(r, k, p);
  fn.resize(k + 1);

  std::cout << '\n' << fn << std::endl;
  return 0;
}
