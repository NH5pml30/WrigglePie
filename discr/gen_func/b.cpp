/* Nikolai Kholiavin, M3238 */
#include <type_traits>
#include <vector>
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
using polynom = std::vector<type>;

template<typename type>
polynom<type> operator+(const polynom<type> &lhs, const polynom<type> &rhs)
{
  polynom<type> res;
  const polynom<type> *min_size = &lhs, *max_size = &rhs;
  if (lhs.size() > rhs.size())
    std::swap(min_size, max_size);
  std::transform(min_size->begin(), min_size->end(), max_size->begin(), std::back_inserter(res), std::plus<>());
  std::copy(max_size->begin() + min_size->size(), max_size->end(), std::back_inserter(res));
  while (!res.empty() && res.back() == type(0))
    res.pop_back();
  return res;
}

template<typename type>
polynom<type> &operator+=(polynom<type> &lhs, const polynom<type> &rhs)
{
  lhs.resize(std::max(lhs.size(), rhs.size()), type(0));
  std::transform(rhs.begin(), rhs.end(), lhs.begin(), lhs.begin(), std::plus<>());
  return lhs;
}

template<typename type>
std::ostream &operator<<(std::ostream &o, const polynom<type> &poly)
{
  for (auto el : poly)
    o << el << ' ';
  return o;
}

template<typename InItl, typename InItr>
auto convolve(InItl lhs_begin, InItl lhs_end,
              InItr rhs_begin, InItr rhs_end, size_t n_)
{
  using type = std::remove_const_t<typename InItl::value_type>;

  ptrdiff_t
    lhs_size = (ptrdiff_t)(lhs_end - lhs_begin),
    rhs_size = (ptrdiff_t)(rhs_end - rhs_begin),
    n = (ptrdiff_t)n_;
  return std::transform_reduce(
    lhs_begin  + std::max(n - rhs_size, ptrdiff_t(0)),
    lhs_begin  + std::min(n, lhs_size),
    std::make_reverse_iterator(rhs_end) + std::max(rhs_size - n, ptrdiff_t(0)),
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
  polynom<type> res;
  std::transform(rhs.begin(), rhs.end(), rhs.begin(),
                 [=](type el) { return el * lhs; });
  return res;
}

template<typename type>
void divide(const polynom<type> &lhs, const polynom<type> &rhs, polynom<type> &res)
{
  assert(rhs.size() > 0 && rhs[0] > 0);
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

template<typename type, typename CoefMorpher>
void compute_substitution(const polynom<type> &p, polynom<type> &res, CoefMorpher &&morpher, type init = type(1))
{
  assert(p.size() > 0 && p[0] == 0);
  std::fill(res.begin(), res.end(), type(0));

  type coef = init;
  polynom<type> powt = {type(1)};
  for (size_t i = 0; i < res.size();
       i++, coef = morpher(coef), powt *= p, powt.resize(res.size(), type(0)))
    res += coef * powt;
}

template<typename type>
void sqrt_one_plus_p(const polynom<type> &p, polynom<type> &res)
{
  return compute_substitution(p, res,
                              [num = type(1), denom = type(2), n = size_t{1}](type coef) mutable {
                                auto new_coef = coef * num / (denom * n);
                                num -= denom;
                                n++;
                                return new_coef;
                              });
}

template<typename type>
void exp(const polynom<type> &p, polynom<type> &res)
{
  return compute_substitution(p, res, [n = size_t{1}](type coef) mutable { return coef / n++; });
}

template<typename type>
void log_one_plus_p(const polynom<type> &p, polynom<type> &res)
{
  return compute_substitution(p, res,
                              [n = type(1), sign = -1](type coef) mutable {
                                return type(sign *= -1) / n++;
                              }, type(0));
}

int main() {
  using poly_t = polynom<mod_int_t>;

  int n, m;
  std::cin >> n >> m;

  poly_t P(n + 1);
  for (auto &el : P)
    std::cin >> el;

  poly_t p1(m);
  sqrt_one_plus_p(P, p1);
  std::cout << p1 << '\n';

  exp(P, p1);
  std::cout << p1 << '\n';

  log_one_plus_p(P, p1);
  std::cout << p1 << '\n';

  return 0;
}
