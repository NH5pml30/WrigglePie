/* Nikolai Kholiavin, M3238 */
#include <type_traits>
#include <vector>
#include <deque>
#include <iostream>
#include <functional>
#include <algorithm>
#include <numeric>
#include <cassert>
#include <string_view>
#include <map>
#include <list>

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

using mod_int_t = modular_number<int64_t, 104'857'601>;

template<typename type>
using polynom = std::list<type>;

template<typename type>
polynom<type> &normalize(polynom<type> &poly)
{
  while (!poly.empty() && poly.back() == type(0))
    poly.pop_back();
  return poly;
}

template<typename type, class BiOp>
void component_wise_helper(const polynom<type> &lhs, const polynom<type> &rhs, polynom<type> &res, BiOp func)
{
  size_t lhs_size = lhs.size(), rhs_size = rhs.size();
  res.resize(std::max(lhs_size, rhs_size));
  size_t min_size = std::min(lhs_size, rhs_size);
  std::transform(lhs.begin(), lhs.begin() + min_size, rhs.begin(), res.begin(), func);
  if (lhs_size > rhs_size)
    std::transform(lhs.begin() + min_size, lhs.end(), res.begin() + min_size,
                   [&](type val) { return func(val, 0); });
  else
    std::transform(rhs.begin() + min_size, rhs.end(), res.begin() + min_size,
                   [&](type val) { return func(0, val); });
  normalize(res);
}

template<typename type, class BiOp>
polynom<type> component_wise(const polynom<type> &lhs, const polynom<type> &rhs, BiOp func)
{
  polynom<type> res;
  component_wise_helper(lhs, rhs, res, std::move(func));
  return res;
}

template<typename type, class BiOp>
polynom<type> &component_wise_assign(polynom<type> &lhs, const polynom<type> &rhs, BiOp func)
{
  component_wise_helper(lhs, rhs, lhs, std::move(func));
  return lhs;
}

template<typename type>
polynom<type> operator+(const polynom<type> &lhs, const polynom<type> &rhs)
{
  return component_wise(lhs, rhs, std::plus<>());
}
template<typename type>
polynom<type> operator-(const polynom<type> &lhs, const polynom<type> &rhs)
{
  return component_wise(lhs, rhs, std::minus<>());
}

template<typename type>
polynom<type> &operator+=(polynom<type> &lhs, const polynom<type> &rhs)
{
  return component_wise_assign(lhs, rhs, std::plus<>());
}

template<typename type>
polynom<type> &operator-=(polynom<type> &lhs, const polynom<type> &rhs)
{
  return component_wise_assign(lhs, rhs, std::minus<>());
}

template<typename type>
std::ostream &operator<<(std::ostream &o, const polynom<type> &poly)
{
  if (!poly.empty())
  {
    o << poly[0];
    std::for_each(poly.begin() + 1, poly.end(), [&](auto &el) { o << ' ' << el; });
  }
  return o;
}

// computes sum(i=0,n)(lhs[i] * rhs[n - i - 1] when i, n - i in range)
template<typename InItl, typename InItr>
auto convolve(InItl lhs_begin, size_t lhs_size_,
              InItr rhs_begin, size_t rhs_size_, size_t n_)
{
  using type = std::remove_const_t<typename InItl::value_type>;

  ptrdiff_t
    lhs_size = (ptrdiff_t)lhs_size_,
    rhs_size = (ptrdiff_t)rhs_size_,
    n = (ptrdiff_t)n_,
    // i >= 0 && i <= lsize - 1 &&
    // n - i - 1 >= 0 && n - i - 1 <= rsize - 1

    // i >= 0 && i >= n - rsize &&
    // i <= lsize - 1 && i <= n - 1

    // i_first[ = max(n - rsize, 0)
    // i_last) = min(lsize, n)
    lhs_offb = std::clamp(n - rhs_size, ptrdiff_t{0}, lhs_size),
    lhs_offe = std::clamp(n, lhs_offb, lhs_size),
    rhs_offb = std::clamp(rhs_size - (n - lhs_offb), ptrdiff_t{0}, rhs_size);
    // is clamped => lhs_offb == lhs_offe and is ok
  auto iter_begin = lhs_begin, iter_end = lhs_begin;
  std::advance(iter_begin, lhs_offb);
  std::advance(iter_end, lhs_offe);
  std::advance(rhs_begin, rhs_size);
  auto rhs_rbegin = std::make_reverse_iterator(rhs_begin);
  std::advance(rhs_rbegin, rhs_offb);
  return std::transform_reduce(iter_begin, iter_end, rhs_rbegin, type(0));
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
  std::transform(lhs.begin(), lhs.end(), std::back_inserter(res),
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
void multiply(const polynom<type> &lhs, const polynom<type> &rhs, polynom<type> &res)
{
  std::generate(res.begin(), res.end(),
                [&, i = (size_t)0]() mutable {
                  ++i;
                  return convolve(lhs.begin(), lhs.size(), rhs.begin(), rhs.size(), i);
                });
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
type fast_recur(polynom<type> first, polynom<type> Q, size_t n)
{
  Q *= type(-1);
  polynom<type> q_mt, dummy;
  for (size_t k = first.size(); n >= k; n /= 2)
  {
    for (size_t i = k; i < 2 * k; i++)
      first.push_back(convolve(std::next(Q.begin()), Q.size() - 1, first.begin(), first.size(), i));
    q_mt = Q;
    std::transform(q_mt.begin(), q_mt.end(), q_mt.begin(), [i = 0](type el) mutable { return (1 - (i++ % 2) * 2) * el; });
    dummy.resize(Q.size() * 2);
    multiply(Q, q_mt, dummy);
    dummy *= type(-1);
    Q.swap(dummy);

    first.remove_if([&, i = 0](auto &) mutable { return i++ % 2 != n % 2; });
    Q.remove_if([i = 0](auto &) mutable { return i++ % 2 == 1; });
  }
  return *std::next(first.begin(), n);
}

int main() {
  using poly_t = polynom<mod_int_t>;

  int k;
  size_t n;
  std::cin >> k >> n;
  poly_t A;
  for (int i = 0; i < k; i++)
  {
    mod_int_t el;
    std::cin >> el;
    A.push_back(el);
  }

  poly_t Q = {1};
  for (int i = 0; i < k; i++)
  {
    mod_int_t el;
    std::cin >> el;
    Q.push_back(0 - el);
  }

  std::cout << fast_recur(A, Q, n - 1).data << std::endl;
  return 0;
}
