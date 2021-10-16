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
  polynom<type> res;
  std::transform(rhs.begin(), rhs.end(), rhs.begin(),
                 [=](type el) { return el * lhs; });
  return res;
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
std::pair<polynom<type>, polynom<type>> compute_pq(const std::vector<type> &recur_start, const std::vector<type> &recur_coefs)
{
  polynom<type> p, q = {1};
  std::transform(recur_coefs.begin(), recur_coefs.end(), std::back_inserter(q), std::negate<>());
  std::transform(recur_start.begin(), recur_start.end(), std::back_inserter(p),
                 [&, i = size_t{0}](type ai) mutable {
                   return ai - convolve(recur_coefs.begin(), recur_coefs.end(), recur_start.begin(),
                                        recur_start.begin() + i, i++);
                 });
  return {std::move(normalize(p)), std::move(normalize(q))};
}

int main() {
  using poly_t = polynom<int64_t>;

  int k;
  std::cin >> k;

  std::vector<int64_t> a(k);
  for (auto &el : a)
    std::cin >> el;

  std::vector<int64_t> c(k);
  for (auto &el : c)
    std::cin >> el;

  auto [p, q] = compute_pq(a, c);

  std::cout << p.size() - 1 << '\n' << p << '\n' << q.size() - 1 << '\n' << q << std::endl;
  return 0;
}
