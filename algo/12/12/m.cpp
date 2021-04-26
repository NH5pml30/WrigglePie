/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <limits>
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

struct trie {
  struct node {
    std::vector<int> term_ids;
    bool visited = false;
  };

  std::vector<node> nodes;
  std::vector<int> suffix_refs;
  using transition_table_t = std::map<char, int>;
  std::vector<transition_table_t> transitions;
  std::vector<int> supersuffix_refs;

  int add_node() {
    nodes.push_back({{}});
    suffix_refs.push_back(0);
    supersuffix_refs.push_back(0);
    transitions.emplace_back(transition_table_t{});
    return (int)nodes.size() - 1;
  }

  template<class Func>
  void bfs(Func &&func) {
    std::set<int> layer = {0};

    do {
      std::set<int> next_layer;
      for (int ind : layer) {
        auto &table = transitions[ind];
        for (auto trans : table)
          if (trans.second > 0)
            next_layer.insert(trans.second);
      }
      for (int ind : layer)
        func(ind);
      layer.swap(next_layer);
    } while (!layer.empty());
  }

  trie(const std::vector<std::string> &dict, const std::set<char> &alphabet) {
    int root = add_node();

    int s_id = 0;
    for (auto &str : dict) {
      int cur = root;
      int i;
      for (i = 0; i < (int)str.size(); i++)
        if (int to = transitions[cur][str[i]]; to > 0)
          cur = to;
        else
          break;
      for (; i < (int)str.size(); i++)
        cur = transitions[cur][str[i]] = add_node();
      nodes[cur].term_ids.push_back(s_id++);
    }

    bfs([&](int k) {
      for (auto ch : alphabet) {
        if (transitions[k][ch] > 0) {
          if (k != 0)
            suffix_refs[transitions[k][ch]] = transitions[suffix_refs[k]][ch];
        } else {
          transitions[k][ch] = transitions[suffix_refs[k]][ch];
        }
      }

      if (!nodes[suffix_refs[k]].term_ids.empty()) {
        supersuffix_refs[k] = suffix_refs[k];
      } else {
        supersuffix_refs[k] = supersuffix_refs[suffix_refs[k]];
      }
    });
  }

  template<class Callback>
  void run(std::string_view sv, Callback &&callback) {
    int v = 0;

    int pos = 0;
    for (auto ch : sv) {
      v = transitions[v][ch];
      int u = !nodes[v].term_ids.empty() ? v : supersuffix_refs[v];
      while (!nodes[u].visited && u > 0) {
        nodes[u].visited = true;
        for (auto id : nodes[u].term_ids)
          callback(pos, id);
        u = supersuffix_refs[u];
      }
    }
  }

  bool is_terminal(int node) const {
    return !nodes[node].term_ids.empty();
  }

  bool is_eps_terminal(int node) const {
    return is_terminal(node) || is_terminal(supersuffix_refs[node]);
  }
};

big_integer count_unterminal_words(const trie &T, const std::set<char> &alphabet, int length) {
  std::vector<big_integer> words_per_node(T.nodes.size()), new_words_per_node(T.nodes.size());
  words_per_node[0] = 1;

  for (int i = 0; i < length; i++) {
    std::fill(new_words_per_node.begin(), new_words_per_node.end(), 0);
    for (int node = 0; node < (int)T.nodes.size(); node++)
      if (!T.is_eps_terminal(node))
        for (auto ch : alphabet)
          new_words_per_node[T.transitions[node].at(ch)] += words_per_node[node];
    words_per_node.swap(new_words_per_node);
  }

  big_integer res = 0;
  for (int node = 0; node < (int)T.nodes.size(); node++)
    if (!T.is_eps_terminal(node))
      res += words_per_node[node];
  return res;
}

int main() {
  int n, m, p;
  std::cin >> n >> m >> p;

  std::set<char> alphabet;
  for (int i = 0; i < n; i++) {
    char ch;
    std::cin >> ch;
    alphabet.insert(ch);
  }

  std::vector<std::string> dict(p);

  for (int i = 0; i < p; i++)
    std::cin >> dict[i];

  trie t(dict, alphabet);

  std::cout << count_unterminal_words(t, alphabet, m) << std::endl;
  return 0;
}
