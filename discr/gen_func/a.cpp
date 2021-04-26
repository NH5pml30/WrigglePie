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

  modular_number(type data) noexcept : data(data % mod) {}
  modular_number() = default;

  friend modular_number operator+(modular_number lhs, modular_number rhs) noexcept {
    return (lhs.data + rhs.data) % mod;
  }

  friend modular_number operator-(modular_number lhs, modular_number rhs) noexcept {
    auto res = (lhs.data - rhs.data) % mod;
    return res < 0 ? mod + res : res;
  }

  friend modular_number operator*(modular_number lhs, modular_number rhs) noexcept {
    return (lhs.data * rhs.data) % mod;
  }

  modular_number &operator+=(modular_number other) noexcept {
    return *this = *this + other;
  }
  modular_number &operator-=(modular_number other) noexcept {
    return *this = *this - other;
  }
  modular_number &operator*=(modular_number other) noexcept {
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
void divide(const polynom<type> &lhs, const polynom<type> &rhs, polynom<type> &res)
{
  assert(rhs.size() > 0 && rhs[0] == type(1));
  // auto lhs_begin = std::find_if(lhs.begin(), lhs.end(), [&](type val) { return val != 0; });
  // auto rhs_begin = std::find_if(rhs.begin(), rhs.end(), [&](type val) { return val != 0; });

  // assert(rhs_begin != rhs.end() && (lhs_begin - lhs.begin()) >= (rhs_begin - rhs.begin));

  std::generate(res.begin(), res.end(), [&, i = (size_t)0]() mutable {
    ++i;
    return type(i > lhs.size() ? 0 : lhs[i - 1]) - convolve(res.begin(), res.end(), rhs.begin(), rhs.end(), i);
  });
}

int main() {
  using poly_t = polynom<mod_int_t>;

  int n, m;
  std::cin >> n >> m;

  poly_t P(n + 1), Q(m + 1);
  for (auto &el : P)
    std::cin >> el;
  for (auto &el : Q)
    std::cin >> el;

  auto sum = P + Q;
  std::cout << sum.size() - 1 << '\n' << sum << '\n';

  auto prod = P * Q;
  std::cout << prod.size() - 1 << '\n' << P * Q << '\n';

  poly_t quotient(1000);
  divide(P, Q, quotient);
  std::cout << quotient << std::endl;
  return 0;
}
