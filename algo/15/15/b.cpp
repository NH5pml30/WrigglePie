/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <cassert>
#include <stack>
#include <optional>
#include <variant>
#include <sstream>
#include <random>
#include <bitset>

template<class It, class Callback, class Accum, class AccumAppend>
void gen_subsets_helper(It begin, It end, Callback &&callback, AccumAppend &&append,
                        Accum accum = {}, int mask = 0, int pos = 0) {
  if (begin == end) {
    callback(mask, accum);
    return;
  }

  gen_subsets_helper(std::next(begin), end, std::forward<Callback>(callback),
                     std::forward<AccumAppend>(append), append(accum, *begin, false), mask,
                     pos + 1);
  gen_subsets_helper(std::next(begin), end, std::forward<Callback>(callback),
                     std::forward<AccumAppend>(append), append(accum, *begin, true),
                     mask | (1 << pos), pos + 1);
}

template<class It, class Callback, class Accum, class AccumAppend>
void gen_subsets(It begin, It end, Callback &&callback, AccumAppend &&append, Accum accum = {}) {
  gen_subsets_helper(begin, end, std::forward<Callback>(callback), std::forward<AccumAppend>(append), accum);
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto &&in = std::cin;

  std::string str;
  in >> str;

  int n = (int)str.size();
  std::vector<bool> s(n);
  for (int i = 0; i < n; i++) {
    s[i] = str[i] == 'x';
  }

  using mask_t = std::pair<uint32_t, int>;
  using accum_t = std::pair<mask_t, mask_t>;

  uint64_t ans = 0;

  auto accum_append = [](accum_t accum, bool val, bool is_chosen) {
    auto [red, blue] = accum;
    if (is_chosen) {
      (red.first <<= 1) |= uint32_t{val};
      red.second++;
    } else {
      (blue.first <<= 1) |= uint32_t{val};
      blue.second++;
    }
    return accum_t{red, blue};
  };

  constexpr int data_bits = 20, int_bits = sizeof(int) * CHAR_BIT;

  std::vector<int32_t> middle2n_red[20], middle2n_blue[20];
  for (auto &v : middle2n_red)
    v.resize(1 << data_bits);
  for (auto &v : middle2n_blue)
    v.resize(1 << data_bits);
  int middle2n_eq = 0;

  auto reverse_byte = [lookup = std::array<unsigned char, 16>{0x0, 0x8, 0x4, 0xc, 0x2, 0xa, 0x6,
                                                              0xe, 0x1, 0x9, 0x5, 0xd, 0x3, 0xb,
                                                              0x7, 0xf}](uint8_t x) -> uint8_t {
    return (uint8_t)((lookup[x & 0xF] << 4) | lookup[x >> 4]);
  };

  auto reverse_int = [&](uint32_t x) -> uint32_t {
    return (reverse_byte(x & 0xFF) << 24) | (reverse_byte((x >> 8) & 0xFF) << 16) |
            (reverse_byte((x >> 16) & 0xFF) << 8) | reverse_byte((uint8_t)((x >> 24) & 0xFF));
  };

  auto reverse_mask = [&](mask_t x) {
    return mask_t{reverse_int(x.first) >> (int_bits - x.second), x.second};
  };

  auto shift_mask = [&](mask_t mask, int n) { return mask_t{mask.first >> n, mask.second - n}; };

  // gen masks for left half
  gen_subsets<>(
      s.begin(), s.begin() + n / 2,
      [&](int, accum_t accum) {
        auto [red, blue] = accum;
        red = reverse_mask(red);
        blue = reverse_mask(blue);
        int min_size = std::min(red.second, blue.second);
        if ((red.first & ((1 << min_size) - 1)) == (blue.first & ((1 << min_size) - 1))) {
          // prefixes matched
          if (red.second > min_size)
            // put middle into red
            middle2n_red[red.second - min_size - 1][red.first >> min_size]++;
          else if (blue.second > min_size)
            // put middle into blue
            middle2n_blue[blue.second - min_size - 1][blue.first >> min_size]++;
          else
            // no middle
            middle2n_eq++;
        }
      },
      accum_append, accum_t{});

  // gen masks for reversed right half
  gen_subsets<>(
      s.rbegin(), s.rbegin() + (n - n / 2),
      [&](int, accum_t accum) {
        auto [red, blue] = accum;
        red = reverse_mask(red);
        blue = reverse_mask(blue);
        int min_size = std::min(red.second, blue.second);
        if ((red.first & ((1 << min_size) - 1)) == (blue.first & ((1 << min_size) - 1))) {
          // prefixes matched
          if (red.second > min_size)
            // red middle, get matched reversed middle from left blue
            ans += middle2n_blue[red.second - min_size - 1]
                                [reverse_mask(shift_mask(red, min_size)).first];
          else if (blue.second > min_size)
            // blue middle, get matched reversed middle from left red
            ans += middle2n_red[blue.second - min_size - 1]
                                [reverse_mask(shift_mask(blue, min_size)).first];
          else
            // no middle
            ans += middle2n_eq;
        }
      },
      accum_append, accum_t{});

  std::cout << ans << std::endl;
  return 0;
}
