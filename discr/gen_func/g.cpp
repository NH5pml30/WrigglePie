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

#include <cstddef>
#include <iosfwd>
#include <cstdint>
#include <cstring>
#include <stdexcept>
#include <sstream>

struct big_integer
{
/* all public functions provide weak exception guarantee (invariant holds) */
private:
  // digit type -- only uint32_t supported because of division
  using place_t = uint32_t;
  using storage_t = std::vector<place_t>;

  // invariant:
  // sign -- highest bit in last place (1 -- negative)
  // data has the smallest size representing the same number in 2's complement form
  storage_t data;
  static constexpr int PLACE_BITS = std::numeric_limits<place_t>::digits;

public:
  big_integer();
  big_integer(const big_integer &other) = default;
  big_integer(int a);
  explicit big_integer(std::string const &str);
  ~big_integer();

  big_integer & operator=(const big_integer &other);

  big_integer & operator+=(const big_integer &rhs);
  big_integer & operator-=(const big_integer &rhs);
  big_integer & operator*=(const big_integer &rhs);
  big_integer & operator/=(const big_integer &rhs);
  big_integer & operator%=(const big_integer &rhs);

  big_integer & operator&=(const big_integer &rhs);
  big_integer & operator|=(const big_integer &rhs);
  big_integer & operator^=(const big_integer &rhs);

  big_integer & operator<<=(int rhs);
  big_integer & operator>>=(int rhs);

  big_integer operator+() const;
  big_integer operator-() const;
  big_integer operator~() const;

  big_integer & operator++();
  big_integer operator++(int);

  big_integer & operator--();
  big_integer operator--(int);

  friend bool operator==(const big_integer &a, const big_integer &b);
  friend bool operator!=(const big_integer &a, const big_integer &b);
  friend bool operator<(const big_integer &a, const big_integer &b);
  friend bool operator>(const big_integer &a, const big_integer &b);
  friend bool operator<=(const big_integer &a, const big_integer &b);
  friend bool operator>=(const big_integer &a, const big_integer &b);

  friend std::string to_string(const big_integer &a);

private:
  explicit big_integer(place_t place);

  /* Operators */
  big_integer & short_multiply(place_t rhs);
  big_integer & long_divide(const big_integer &rhs, big_integer &rem);
  big_integer & short_divide(place_t rhs, place_t &rem);
  big_integer & bit_shift(int bits);
  static int compare(const big_integer &l, const big_integer &r);

  /* Invariant-changing functions */
  // corrects sign & invariant
  big_integer & correct_sign_bit(bool expected_sign_bit, place_t carry = 0);
  // corrects invariant
  big_integer & shrink();
  // destroys invariant, inflating data
  void resize(size_t new_size);

  /* Non-invariant-changing function */
  bool make_absolute();
  big_integer & revert_sign(bool sign);

  int sign() const;
  bool sign_bit() const;

  size_t size() const;
  size_t unsigned_size() const;
  place_t default_place() const;
  place_t get_or_default(ptrdiff_t at) const;

  /* Useful iterating functions */
  using binary_operator = std::function<place_t (place_t, place_t)>;
  using unary_operator = std::function<place_t (place_t)>;
  using unary_consumer = std::function<void (place_t)>;

  void iterate(const big_integer &b, const binary_operator &action);
  void iterate(const unary_operator &action);
  void iterate_r(const unary_operator &action);
  void iterate(const unary_consumer &action) const;
  void iterate_r(const unary_consumer &action) const;
  big_integer & place_wise(const big_integer &b,
    const binary_operator &action);
};

big_integer operator+(big_integer a, const big_integer &b);
big_integer operator-(big_integer a, const big_integer &b);
big_integer operator*(big_integer a, const big_integer &b);
big_integer operator/(big_integer a, const big_integer &b);
big_integer operator%(big_integer a, const big_integer &b);

big_integer operator&(big_integer a, const big_integer &b);
big_integer operator|(big_integer a, const big_integer &b);
big_integer operator^(big_integer a, const big_integer &b);

big_integer operator<<(big_integer a, int b);
big_integer operator>>(big_integer a, int b);

bool operator==(const big_integer &a, const big_integer &b);
bool operator!=(const big_integer &a, const big_integer &b);
bool operator<(const big_integer &a, const big_integer &b);
bool operator>(const big_integer &a, const big_integer &b);
bool operator<=(const big_integer &a, const big_integer &b);
bool operator>=(const big_integer &a, const big_integer &b);

std::string to_string(const big_integer &a);
std::ostream & operator<<(std::ostream &s, const big_integer &a);

/* Basis for big integer functions */
template<typename type>
  bool sign_bit(type val) { return val >> (std::numeric_limits<type>::digits - 1); }

bool big_integer::sign_bit() const { return ::sign_bit(data.back()); }

template<typename type>
static type default_place(bool bit) { return bit ? std::numeric_limits<type>::max() : 0; }

big_integer::place_t big_integer::default_place() const
{
  return ::default_place<place_t>(sign_bit());
}

big_integer::place_t big_integer::get_or_default(ptrdiff_t at) const
{
  if (at < 0) return 0;
  if (static_cast<size_t>(at) >= data.size()) return default_place();
  return data[at];
}

big_integer & big_integer::shrink()
{
  // make sure invariant holds
  while (data.size() > 1 && data.back() == default_place() &&
         sign_bit() == ::sign_bit(data[data.size() - 2]))
    // while last place is default and no change in sign
    data.pop_back();
  return *this;
}

void big_integer::resize(size_t new_size) { data.resize(new_size, default_place()); }
size_t big_integer::size() const { return data.size(); }

size_t big_integer::unsigned_size() const
{
  return std::max(size_t{1}, data.size() - (data.back() == default_place()));
}

big_integer & big_integer::correct_sign_bit(bool expected_sign_bit, place_t carry)
{
  // invariant does not hold for now
  // zero is always accepted, but could be non-shrinked
  try
  {
    if (carry != 0)
      data.push_back(carry);
    if (sign_bit() != expected_sign_bit && !(expected_sign_bit &&
      std::all_of(data.begin(), data.end(), [](place_t i) { return i == 0; })))
      data.push_back(::default_place<place_t>(expected_sign_bit));
  }
  catch (...)
  {
    // exception thrown, but maybe invariant doesn't hold
    shrink();
    throw;
  }
  return shrink();
}

big_integer & big_integer::revert_sign(bool sign)
{
  // if *this == 0 nothing changes
  if (sign_bit() != sign)
    *this = - *this;
  return *this;
}

bool big_integer::make_absolute()
{
  bool sign = sign_bit();
  revert_sign(0);
  return sign;
}

void big_integer::iterate(const big_integer &b, const binary_operator &action)
{
  resize(std::max(data.size(), b.data.size()));
  auto l_end = data.end();
  auto r_end = b.data.end(), r_it = b.data.begin();
  for (auto it = data.begin(); it != l_end; it++)
  {
    *it = action(*it, r_it >= r_end ? b.default_place() : *r_it);
    if (r_it != r_end) r_it++;
  }
}
void big_integer::iterate(const unary_operator &action)
{
  auto end = data.end();
  for (auto it = data.begin(); it != end; it++)
    *it = action(*it);
}
void big_integer::iterate_r(const unary_operator &action)
{
  auto begin = data.begin();
  for (auto it = data.end(); it != begin; it--)
    *(it - 1) = action(*(it - 1));
}
void big_integer::iterate(const unary_consumer &action) const
{
  auto end = data.end();
  for (auto it = data.begin(); it != end; it++)
    action(*it);
}
void big_integer::iterate_r(const unary_consumer &action) const
{
  auto begin = data.begin();
  for (auto it = data.end(); it != begin; it--)
    action(*(it - 1));
}

big_integer & big_integer::place_wise(const big_integer &b,
  const binary_operator &action)
{
  iterate(b, action);
  return shrink();
}

/***
 * Major functions for big_integer
 ***/

big_integer::big_integer() : data(1, place_t{0})
{}

// because digits are stored in 2's complement, initializing is done properly
big_integer::big_integer(int a) : data(1, static_cast<place_t>(a))
{}

big_integer::big_integer(place_t place) : data(1, place)
{
  // must be unsigned
  correct_sign_bit(0);
}

big_integer::big_integer(const std::string &str) : data(1, place_t{0})
{
  auto it = str.cbegin();
  bool is_negated = false;
  if (it != str.cend() && *it == '-')
  {
    is_negated = true;
    it++;
  }

  if (it == str.cend())
    throw std::runtime_error("Cannot read number from string: '" + str + "'");

  for (; it != str.cend(); it++)
  {
    short_multiply(10);
    *this += *it - '0';
  }

  revert_sign(is_negated);
}

big_integer::~big_integer()
{
}

big_integer & big_integer::operator=(const big_integer &other)
{
  data = other.data;
  return *this;
}

/***
 * Arithmetic functions for 32/64-bit numbers
 **/

template<typename type>
  static type addc(type left, type right, bool &carry)
  {
    static constexpr auto limit_max = std::numeric_limits<type>::max();
    bool old_carry = carry;
    carry = (limit_max - left < right ||
             (left == limit_max && carry == 1) ||
             limit_max - left - carry < right);
    return static_cast<type>(left + right + old_carry);
  }

static inline uint32_t low_bytes(uint64_t x) { return x & 0xFFFFFFFF; }
static inline uint32_t high_bytes(uint64_t x) { return static_cast<uint32_t>(x >> 32); }
static inline uint16_t low_bytes(uint32_t x) { return x & 0xFFFF; }
static inline uint16_t high_bytes(uint32_t x) { return static_cast<uint16_t>(x >> 16); }

static std::pair<uint64_t, uint64_t> mul(uint64_t left, uint64_t right)
{
  uint64_t left_low = low_bytes(left);
  uint64_t right_low = low_bytes(right);
  uint64_t left_high = high_bytes(left);
  uint64_t right_high = high_bytes(right);
  uint64_t bd = left_low * right_low;
  uint64_t ad = left_high * right_low;
  uint64_t bc = left_low * right_high;
  uint64_t ac = left_high * right_high;

  // res = ac * 2^64 + (ad + bc) * 2^32 + bd

  uint32_t carry = high_bytes(uint64_t{high_bytes(bd)} + low_bytes(bc) + low_bytes(ad));
  return
  {
    bd + (ad << 32) + (bc << 32),
    ac + high_bytes(ad) + high_bytes(bc) + carry
  };
}

static std::pair<uint32_t, uint32_t> mul(uint32_t left, uint32_t right)
{
  uint64_t res = uint64_t{left} * right;
  return {low_bytes(res), high_bytes(res)};
}

static bool less_3_digits(uint64_t lhs_low, uint32_t lhs_high,
                          uint64_t rhs_low, uint32_t rhs_high)
{
  return lhs_high < rhs_high || (lhs_high == rhs_high && lhs_low < rhs_low);
}

static uint64_t get_3_digits(uint64_t low, uint32_t high, int at)
{
  switch (at)
  {
  case -1:
    return high;
  case 0:
    return (uint64_t{high} << 16) | (low >> 48);
  case 1:
    return (uint64_t{low_bytes(high)} << 32) | (low >> 32);
  }
  return 0;
}

static std::pair<uint64_t, uint32_t> sub_5_digits(uint64_t lhs_low, uint32_t lhs_high,
                                                  uint64_t rhs_low, uint16_t rhs_high, int at)
{
  if (at == 0)
  {
    uint64_t l64 = (uint64_t{low_bytes(lhs_high)} << 48)| (lhs_low >> 16);
    bool borrow = 0;
    l64 = addc(l64, ~rhs_low + 1, borrow);
    uint16_t l16 = high_bytes(lhs_high);
    l16 = static_cast<uint16_t>(l16 - borrow); // cringe
    borrow = 0;
    l16 = addc(l16, (uint16_t)(~rhs_high + 1), borrow);
    lhs_high = (uint32_t{l16} << 16) | (uint32_t)(l64 >> 48);
    lhs_low = (l64 << 16) | low_bytes(lhs_low);
  }
  else
  {
    bool borrow = 0;
    lhs_low = addc(lhs_low, ~rhs_low + 1, borrow);
    lhs_high -= borrow;
    borrow = 0;
    lhs_high = addc(lhs_high, (uint32_t)(~rhs_high + 1), borrow);
  }
  return {lhs_low, lhs_high};
}

static std::pair<uint32_t, uint64_t> div3_2(uint32_t lhs_low, uint32_t lhs_med, uint32_t lhs_high,
                                            uint32_t rhs_low, uint32_t rhs_high)
{
  // divide in base 2^16 with trial digits on prefixes
  uint64_t
    r_low64 = (uint64_t{lhs_med} << 32) | lhs_low,
    d = (uint64_t{rhs_high} << 32) | rhs_low;
  uint32_t
    r_high = lhs_high,
    d2 = rhs_high;

  if (get_3_digits(r_low64, r_high, -1) / d2 != 0)
    // overflow
    return {std::numeric_limits<uint32_t>::max(), 0};

  // high trial digit
  uint16_t qt1 = (uint16_t)(get_3_digits(r_low64, r_high, 0) / d2);
  auto dq = mul(d, uint64_t{qt1});
  if (less_3_digits(r_low64, r_high, dq.first << 16, low_bytes((dq.second << 16) | (dq.first >> 48))))
  {
    // correct estimate
    qt1--;
    dq = mul(d, uint64_t{qt1});
  }
  // subtract from remainder
  auto rmdq = sub_5_digits(r_low64, r_high, dq.first, (uint16_t)dq.second, 0);
  r_low64 = rmdq.first;
  r_high = rmdq.second;

  // low trial digit
  uint16_t qt2 = (uint16_t)(get_3_digits(r_low64, r_high, 1) / d2);
  dq = mul(d, uint64_t{qt2});
  if (less_3_digits(r_low64, r_high, dq.first, low_bytes(dq.second)))
  {
    // correct estimate
    qt2--;
    dq = mul(d, uint64_t{qt2});
  }
  // subtract from remainder
  rmdq = sub_5_digits(r_low64, r_high, dq.first, (uint16_t)dq.second, 1);
  r_low64 = rmdq.first;
  r_high = rmdq.second;

  return {(uint32_t{qt1} << 16) | qt2, r_low64};
}

static std::pair<uint32_t, uint32_t> div2_1(uint32_t lhs_low, uint32_t lhs_high, uint32_t rhs)
{
  uint64_t lhs = ((uint64_t{lhs_high} << 32) | lhs_low);
  return {(uint32_t)(lhs / rhs), (uint32_t)(lhs % rhs)};
}

/***
 * Rest of arithmetic operators for big_integer
 ***/

big_integer & big_integer::operator+=(const big_integer &rhs)
{
  bool old_sign = sign_bit(), rhs_sign = rhs.sign_bit();
  bool carry = 0;
  iterate(rhs, [&](place_t l, place_t r) { return addc(l, r, carry); });
  if (old_sign == rhs_sign)
    correct_sign_bit(old_sign);

  // 1... + 1... => 0... + carry  | new place
  //                1... + carry  |
  // 0... + 0... => 1... no carry | new place
  //                0... no carry |
  // 1... + 0... => 1... no carry |
  //                0... + carry  |

  return shrink();
}

big_integer & big_integer::operator-=(const big_integer &rhs)
{
  return *this += -rhs;
}

// multiplication by a positive integer that fits into place_t
big_integer & big_integer::short_multiply(place_t rhs)
{
  bool old_sign = make_absolute();

  place_t carry = 0;
  iterate([&](place_t datai)
    {
      auto res_carry = mul(datai, rhs);
      bool add_carry = false;
      place_t result = addc(res_carry.first, carry, add_carry);
      carry = addc(res_carry.second, place_t{ 0 }, add_carry);
      return result;
    });
  correct_sign_bit(0, carry);
  return revert_sign(old_sign);
}

big_integer & big_integer::operator*=(const big_integer &rhs)
{
  bool sign = make_absolute() ^ rhs.sign_bit();
  big_integer right = rhs.sign_bit() ? -rhs : rhs;

  big_integer res = 0;
  int i = 0;
  right.iterate([&](place_t rdatai)
    {
      res += big_integer(*this).short_multiply(rdatai) << ((int)i * PLACE_BITS);
      i++;
    });
  return *this = res.revert_sign(sign);
}

// division by a positive integer that fits into place_t,
// (requires place_t to be uint32_t because of div2_1)
big_integer & big_integer::short_divide(place_t rhs, place_t &rem)
{
  rem = 0;
  iterate_r([&](place_t datai)
    {
      auto res_rem = div2_1(datai, rem, rhs);
      rem = res_rem.second;
      return res_rem.first;
    });
  return shrink();
}

// long division using algorithm with trial digits and division of prefixes
// (requires place_t to be uint32_t because of short_divide, div2_1 & div3_2)
big_integer & big_integer::long_divide(const big_integer &rhs, big_integer &rem)
{
  bool this_sign = make_absolute(), sign = this_sign ^ rhs.sign_bit();

  big_integer right = rhs.sign_bit() ? -rhs : rhs;

  size_t n = unsigned_size(), m = right.unsigned_size();

  if (m == 1)
  {
    place_t r;
    short_divide(right.data[0], r);
    rem = big_integer(r);
  }
  else if (m > n)
  {
    // divisor is greater than dividend, quotient is 0, remainder is dividend
    rem = *this;
    *this = 0;
  }
  else
  {
    // divide with base 2^PLACE_BITS
    // 2 <= m <= n -- true

    // copy operands: starting remainder is this, divisor is right
    rem = *this;
    big_integer d = right;

    // normalize divisor d (largest place >= base / 2)
    place_t f = d.data[m - 1] == std::numeric_limits<place_t>::max() ?
      1 : div2_1(0, 1, d.data[m - 1] + 1).first;
    rem.short_multiply(f);
    d.short_multiply(f);

    // 2 leading digits of divisor for quotient digits estimate
    place_t d2_high = d.data[m - 1];
    place_t d2_low = d.data[m - 2];
    // compute quotient digits
    std::vector<place_t> new_data(n - m + 1);
    for (size_t ki = 0; ki <= n - m; ki++)
    {
      int k = (int)(n - m - ki);
      // first, count 3 leading digits of remainder
      place_t r3_high = k + m >= rem.data.size() ? 0 : rem.data[k + m];
      place_t r3_med = k + m - 1 >= rem.data.size() ? 0 : rem.data[k + m - 1];
      place_t r3_low = k + m - 2 >= rem.data.size() ? 0 : rem.data[k + m - 2];
        // obtain k-th digit estimate
      place_t qt = div3_2(r3_low, r3_med, r3_high, d2_low, d2_high).first;
      // count result with estimate
      big_integer dq = big_integer(d).short_multiply(qt) <<= k * PLACE_BITS;
      if (rem < dq)
      {
        // wrong, correct estimate
        qt--;
        dq = big_integer(d).short_multiply(qt) <<= k * PLACE_BITS;
      }
      // set digit in quotient
      new_data[k] = qt;
      // subtract current result from remainder
      rem -= dq;
    }
    place_t dummy;
    rem.short_divide(f, dummy);
    data = storage_t(new_data);
    correct_sign_bit(0);
  }
  rem.revert_sign(this_sign);
  return revert_sign(sign);
}

big_integer & big_integer::operator/=(const big_integer &rhs)
{
  big_integer dummy;
  return long_divide(rhs, dummy);
}

big_integer & big_integer::operator%=(const big_integer &rhs)
{
  big_integer(*this).long_divide(rhs, *this);
  return *this;
}

big_integer & big_integer::operator&=(const big_integer &rhs)
{
  return place_wise(rhs, std::bit_and<place_t>());
}

big_integer & big_integer::operator|=(const big_integer &rhs)
{
  return place_wise(rhs, std::bit_or<place_t>());
}

big_integer & big_integer::operator^=(const big_integer &rhs)
{
  return place_wise(rhs, std::bit_xor<place_t>());
}

big_integer & big_integer::operator<<=(int rhs)
{
  return bit_shift(rhs);
}

big_integer & big_integer::operator>>=(int rhs)
{
  return bit_shift(-rhs);
}

big_integer & big_integer::bit_shift(int rhs)
{
  // rhs > 0 -> left shift
  // rhs < 0 -> right shift

  // [lsrc][rsrc]
  //       <-> bits
  //    [plce]
  bool sign = sign_bit();
  int places = rhs / PLACE_BITS, bits = rhs % PLACE_BITS;
  if (rhs < 0 && bits != 0)
    places--, bits += PLACE_BITS;
  std::vector<place_t> new_data(std::max(data.size() + places + (bits > 0), size_t{1}),
                                get_or_default(rhs < 0 ?
                                  static_cast<ptrdiff_t>(data.size()) : -1));
  for (size_t i = std::max(places, 0); i < data.size() + places + (bits > 0); i++)
  {
    // handle PLACE_BITS shift undefined behavior with cases
    if (bits == 0)
      new_data[i] = data[i - places];
    else
    {
      place_t
        lsrc = get_or_default(static_cast<ptrdiff_t>(i) - places),
        rsrc = get_or_default(static_cast<ptrdiff_t>(i) - places - 1);
      new_data[i] = (lsrc << bits) | (rsrc >> (PLACE_BITS - bits));
    }
  }
  data = storage_t(new_data);
  return correct_sign_bit(sign);
}

big_integer big_integer::operator+() const
{
  return *this;
}

big_integer big_integer::operator-() const
{
  return ~ *this + 1;
}

big_integer big_integer::operator~() const
{
  return big_integer().place_wise(*this, [](place_t, place_t y) { return ~y; });
}

big_integer & big_integer::operator++()
{
  return *this += 1;
}

big_integer big_integer::operator++(int)
{
  big_integer r = *this;
  ++ *this;
  return r;
}

big_integer & big_integer::operator--()
{
  return *this -= 1;
}

big_integer big_integer::operator--(int)
{
  big_integer r = *this;
  -- *this;
  return r;
}

big_integer operator+(big_integer a, const big_integer &b)
{
  return a += b;
}

big_integer operator-(big_integer a, const big_integer &b)
{
  return a -= b;
}

big_integer operator*(big_integer a, const big_integer &b)
{
  return a *= b;
}

big_integer operator/(big_integer a, const big_integer &b)
{
  return a /= b;
}

big_integer operator%(big_integer a, const big_integer &b)
{
  return a %= b;
}

big_integer operator&(big_integer a, const big_integer &b)
{
  return a &= b;
}

big_integer operator|(big_integer a, const big_integer &b)
{
  return a |= b;
}

big_integer operator^(big_integer a, const big_integer &b)
{
  return a ^= b;
}

big_integer operator<<(big_integer a, int b)
{
  return a <<= b;
}

big_integer operator>>(big_integer a, int b)
{
  return a >>= b;
}

int big_integer::sign() const
{
  if (sign_bit())
    return -1;
  if (data.size() == 1 && data[0] == 0)
    return 0;
  return 1;
}

int big_integer::compare(const big_integer &l, const big_integer &r)
{
  bool lsign = l.sign_bit(), rsign = r.sign_bit();
  if (lsign != rsign)
    return rsign - lsign;
  int sign = lsign ? -1 : 1;
  if (l.unsigned_size() != r.unsigned_size())
    return l.unsigned_size() > r.unsigned_size() ? sign : -sign;
  const place_t *ldata = l.data.data(), *rdata = r.data.data();
  size_t s = l.unsigned_size();
  for (size_t i = 0; i < s; i++)
  {
    place_t a = ldata[s - i - 1], b = rdata[s - i - 1];
    if (a != b)
      return a > b ? 1 : -1;
  }
  return 0;
}

bool operator==(const big_integer &a, const big_integer &b)
{
  return a.data == b.data;
}

bool operator!=(const big_integer &a, const big_integer &b)
{
  return a.data != b.data;
}

bool operator<(const big_integer &a, const big_integer &b)
{
  return big_integer::compare(a, b) < 0;
}

bool operator>(const big_integer &a, const big_integer &b)
{
  return big_integer::compare(a, b) > 0;
}

bool operator<=(const big_integer &a, const big_integer &b)
{
  return big_integer::compare(a, b) <= 0;
}

bool operator>=(const big_integer &a, const big_integer &b)
{
  return big_integer::compare(a, b) >= 0;
}

std::string to_string(const big_integer &a)
{
  big_integer c = a;
  bool sgn = c.make_absolute();
  std::string reverse;
  if (c == 0)
    reverse.push_back('0');
  while (c != 0)
  {
    uint32_t rem;
    c.short_divide(10, rem);
    reverse += static_cast<char>((static_cast<char>(rem) + '0'));
  }
  if (sgn)
    reverse.push_back('-');

  return std::string(reverse.rbegin(), reverse.rend());
}

std::ostream & operator<<(std::ostream &s, const big_integer &a)
{
  return s << to_string(a);
}

template<typename type>
using polynom = std::deque<type>;

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
auto convolve(InItl lhs_begin, InItl lhs_end,
              InItr rhs_begin, InItr rhs_end, size_t n_)
{
  using type = std::remove_const_t<typename InItl::value_type>;

  ptrdiff_t
    lhs_size = (ptrdiff_t)(lhs_end - lhs_begin),
    rhs_size = (ptrdiff_t)(rhs_end - rhs_begin),
    n = (ptrdiff_t)n_;
  type res = 0;
  for (ptrdiff_t i = 0; i < n; i++)
    if (i < lhs_size && n - i - 1 >= 0 && n - i - 1 < rhs_size)
      res += *(lhs_begin + i) * *(rhs_begin + (n - i - 1));
  return res;

  /*
  ptrdiff_t
    lhs_size = (ptrdiff_t)(lhs_end - lhs_begin),
    rhs_size = (ptrdiff_t)(rhs_end - rhs_begin),
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
  return std::transform_reduce(
    lhs_begin + lhs_offb,
    lhs_begin + lhs_offe,
    std::make_reverse_iterator(rhs_end) + rhs_offb,
    type(0));
  */
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
                  return convolve(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), i);
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

namespace first_coefs
{
  template<typename type>
  type comb_with_repitions(type n, type k)
  {
    n = n + k - 1;

    type res = 1;
    for (type i = 0; i < k; ++i)
    {
      res *= n - i;
      res /= i + 1;
    }
    return res;
  }

  template<typename type, size_t n_of_coefs>
  polynom<type> mset(const polynom<type> &a_first)
  {
    // assert(a_first[0] == 0);
    std::vector<std::vector<type>> sum_n_each_leq_k(n_of_coefs);
    for (auto &v : sum_n_each_leq_k)
      v.resize(n_of_coefs);
    auto get = [&](ptrdiff_t n, ptrdiff_t k) -> type {
      if (n >= 0 && n < n_of_coefs && k >= 0 && k < n_of_coefs)
        return sum_n_each_leq_k[n][k];
      return 0;
    };
    std::fill(sum_n_each_leq_k[0].begin(), sum_n_each_leq_k[0].end(), 1);
    for (size_t n = 0; n < n_of_coefs; n++)
    {
      for (size_t k = 1; k <= n; k++)
        for (size_t i = 0; i <= n / k; i++)
          sum_n_each_leq_k[n][k] += comb_with_repitions(k < a_first.size() ? a_first[k] : 0, type(i)) * get(n - i * k, k - 1);
      std::fill(sum_n_each_leq_k[n].begin() + n + 1, sum_n_each_leq_k[n].end(), sum_n_each_leq_k[n][n]);
    }

    polynom<type> result;
    for (auto &sum_n : sum_n_each_leq_k)
      result.push_back(sum_n.back());
    return result;
  }

  template<typename type, size_t n_of_coefs>
  polynom<type> pair(const polynom<type> &a_first, const polynom<type> &b_first)
  {
    polynom<type> res(n_of_coefs);
    multiply(a_first, b_first, res);
    return res;
  }

  template<typename type, size_t n_of_coefs>
  polynom<type> list(const polynom<type> &a_first)
  {
    // assert(a_first[0] == 0);
    polynom<type> res(n_of_coefs), powa = a_first, mult = a_first;
    mult[0] = powa[0] = 0;
    res[0] = 1;
    while (std::find_if(powa.begin(), powa.end(), [](auto el) { return el != 0; }) != powa.end())
    {
      res += powa;

      polynom<type> dummy(n_of_coefs);
      multiply(powa, mult, dummy);
      powa.swap(dummy);
    }
    res.resize(n_of_coefs);
    return res;

    /*
    to_divide *= type(-1);
    to_divide[0] = 1;
    divide({type(1)}, to_divide, res);
    return res;
    */
  }
}

using int_t = uint64_t;
using expr_data = polynom<int_t>;
constexpr size_t n_of_coefs = 7;

std::string_view &expect(std::string_view &sv, char ch)
{
  if (sv.empty() || sv[0] != ch)
    throw "up";
  sv.remove_prefix(1);
  return sv;
}

std::string_view &expect(std::string_view &sv, const std::string &str)
{
  if (sv.size() < str.size() || sv.substr(0, str.size()).compare(str) != 0)
    throw "up";
  sv.remove_prefix(str.size());
  return sv;
}

expr_data parse_string(std::string_view &sv)
{
  if (sv.empty())
    throw "up";

  static const std::map<char, expr_data (*)(const expr_data &, const expr_data &)> binary_ops = {
      std::make_pair('P', &first_coefs::pair<int_t, n_of_coefs>),
  };
  static const std::map<char, expr_data (*)(const expr_data &)> unary_ops = {
      std::make_pair('L', &first_coefs::list<int_t, n_of_coefs>),
      std::make_pair('S', &first_coefs::mset<int_t, n_of_coefs>),
  };
  static const std::map<char, expr_data> constants = {std::make_pair('B', expr_data({0, 1}))};

  if (auto iter = constants.find(sv.front()); iter != constants.end())
  {
    sv.remove_prefix(1);
    return iter->second;
  }
  else if (auto iter = unary_ops.find(sv.front()); iter != unary_ops.end())
  {
    sv.remove_prefix(1);
    auto arg = parse_string(expect(sv, '('));
    expect(sv, ')');
    return iter->second(arg);
  }
  else if (auto iter = binary_ops.find(sv.front()); iter != binary_ops.end())
  {
    sv.remove_prefix(1);
    auto lhs = parse_string(expect(sv, '('));
    auto rhs = parse_string(expect(sv, ','));
    expect(sv, ')');
    return iter->second(lhs, rhs);
  }
  throw "up";
}

int main() {
  std::string str;
  std::cin >> str;

  std::string_view sv = str;

  auto res = parse_string(sv);
  if (!sv.empty())
    throw "up";
  res.resize(n_of_coefs);
  std::cout << res << std::endl;

  return 0;
}
