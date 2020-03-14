/* Kholiavin Nikolai, M3138 */

#include <algorithm>
#include <array>
#include <cstddef>
#include <functional>
#include <iomanip>
#include <iostream>
#include <random>
#include <vector>

enum class op : unsigned char {
  NOP = 0x00,
  ADD = 0x01,
  SUB = 0x02,
  MUL = 0x03,
  DIV = 0x04,
  MOD = 0x05,
  AND = 0x10,
  OR = 0x11,
  XOR = 0x12,
  NEG = 0x20,
  NOT = 0x21,
  LOAD = 0x30,
  PUT = 0x31,
  JMP = 0x40,
  JZ = 0x41,
  JNZ = 0x42,
  JG = 0x43,
  JGE = 0x44,
  JL = 0x45,
  JLE = 0x46,
  RET = 0xFF
};

template <bool only_count, typename type>
size_t write_bytes(std::byte *&data, const type &val) {
  if constexpr (!only_count) {
    *(type *)data = val;
    data += sizeof(type);
  }
  return sizeof(type);
}

template <bool only_count, typename type>
size_t write_bytes(std::byte *&data, const type *bytes, size_t n) {
  static_assert(1 || bytes);
  if constexpr (!only_count) {
    memcpy(data, bytes, n * sizeof(type));
    data += n * sizeof(type);
  }
  return n * sizeof(type);
}

template <bool only_count>
size_t write_load(std::byte *&data, uint8_t dst, uint8_t addr) {
  size_t res = 0;
  res += write_bytes<only_count>(data, op::LOAD);
  res += write_bytes<only_count>(data, dst);
  res += write_bytes<only_count>(data, addr);
  return res;
}

template <bool only_count>
size_t write_add(std::byte *&data, uint8_t src_l, uint8_t src_r, uint8_t dst) {
  size_t res = 0;
  res += write_bytes<only_count>(data, op::ADD);
  res += write_bytes<only_count>(data, src_l);
  res += write_bytes<only_count>(data, src_r);
  res += write_bytes<only_count>(data, dst);
  return res;
}

template <bool only_count>
size_t write_mul(std::byte *&data, uint8_t src_l, uint8_t src_r,
                 uint8_t dst_low, uint8_t dst_high) {
  size_t res = 0;
  res += write_bytes<only_count>(data, op::MUL);
  res += write_bytes<only_count>(data, src_l);
  res += write_bytes<only_count>(data, src_r);
  res += write_bytes<only_count>(data, dst_low);
  res += write_bytes<only_count>(data, dst_high);
  return res;
}

template <bool only_count>
size_t write_mod(std::byte *&data, uint8_t src_l_low, uint8_t src_r,
                 uint8_t dst, uint8_t src_l_high) {
  size_t res = 0;
  res += write_bytes<only_count>(data, op::MOD);
  res += write_bytes<only_count>(data, src_l_low);
  res += write_bytes<only_count>(data, src_r);
  res += write_bytes<only_count>(data, dst);
  res += write_bytes<only_count>(data, src_l_high);
  return res;
}

template <bool only_count>
size_t write_put(std::byte *&data, uint8_t dst, uint32_t bytes) {
  size_t res = 0;
  res += write_bytes<only_count>(data, op::PUT);
  res += write_bytes<only_count>(data, dst);
  res += write_bytes<only_count>(data, bytes);
  return res;
}

template <bool only_count>
size_t write_ret(std::byte *&data) {
  size_t res = 0;
  res += write_bytes<only_count>(data, op::RET);
  return res;
}

class hash_function {
 private:
  uint32_t p, a, b, M;
  static std::default_random_engine generator;
  static uint32_t next_pow2(uint32_t x) {
    int i;
    for (i = sizeof(uint32_t) * 8 - 1; i >= 0; i--)
      if ((x >> i) & 1) break;
    return (uint32_t)1 << (i + 1);
  }

  static constexpr uint32_t PRIME_MAX = 101000;

  static std::vector<uint32_t> count_primes() {
    std::vector<uint32_t> result;
    std::vector<bool> flags(PRIME_MAX + 1);
    for (uint32_t i = 2; i <= PRIME_MAX; i++) {
      if (!flags[i]) {
        for (uint32_t j = 2 * i; j <= PRIME_MAX; j += i) flags[j] = true;
      }
    }

    result.reserve(10000);
    for (uint32_t i = 2; i <= PRIME_MAX; i++)
      if (!flags[i]) result.push_back(i);
    return result;
  }

  static uint32_t find_next_prime(uint32_t x) {
    if (x < 31) return 31;
    if (x < primes.front() || x >= primes.back()) return 1'000'000'007;
    int L = -1, R = (int)primes.size();
    while (L < R - 1) {
      int M = (L + R) / 2;
      if (primes[M] > x)
        R = M;
      else
        L = M;
    }
    return primes[R];
  }

  static std::vector<uint32_t> primes;

 public:
  void recreate(uint32_t M) {
    this->M = M;
    p = find_next_prime(M + std::uniform_int_distribution<uint32_t>(0, 100)(generator));
    std::uniform_int_distribution<uint32_t> a_distr(1, p - 1),
        b_distr(0, p - 1);
    a = a_distr(generator), b = b_distr(generator);
  }

  bool has_collisions(const std::vector<int> a) {
    for (size_t i = 0; i < a.size(); i++) {
      for (size_t j = i + 1; j < a.size(); j++) {
        if (count(a[i]) == count(a[j])) return true;
      }
    }
    return false;
  }

  hash_function(uint32_t M) { recreate(M); }

  size_t count(int x) { return (x * a + b) % p % M; }

  size_t data_bytes_size() {
    return 4 * 3;  // a, b, p
  }

  template <bool only_count>
  size_t to_data_bytes(std::byte *&data) {
    size_t res = 0;
    res += write_bytes<only_count>(data, uint32_t{a});
    res += write_bytes<only_count>(data, uint32_t{b});
    res += write_bytes<only_count>(data, uint32_t{p});
    return res;
  }

  // pre:
  //   r0 -- x
  //   r1 -- M
  //   r2 -- memory address slot
  //   r4 -- 4
  //   rbegin_null..r255 -- 0
  // modifies:
  //   r2, r3, r6
  // post:
  //   r6 -- h(x)
  //   r2 -- moved past local data
  //   rbegin_null..r255 -- 0
  template <bool only_count>
  static size_t to_instr_bytes(std::byte *&data, uint8_t &begin_null) {
    size_t res = 0;

    /* r3 = a */
    res += write_load<only_count>(data, 3, 2);
    /* r2 += 4 */
    res += write_add<only_count>(data, 2, 4, 2);

    /* (r6, r5(assumed 0)) = x * a */
    res += write_mul<only_count>(data, 0, 3, 6, begin_null++);

    /* r3 = b */
    res += write_load<only_count>(data, 3, 2);
    /* r2 += 4 */
    res += write_add<only_count>(data, 2, 4, 2);

    /* r6 += b */
    res += write_add<only_count>(data, 6, 3, 6);

    /* r3 = p */
    res += write_load<only_count>(data, 3, 2);
    /* r2 += 4 */
    res += write_add<only_count>(data, 2, 4, 2);

    /* (r6, r5(assumed 0)) %= p */
    res += write_mod<only_count>(data, 6, 3, 6, begin_null++);

    /* (r6, r5(assumed 0)) %= M */
    res += write_mod<only_count>(data, 6, 1, 6, begin_null++);

    return res;
  }

  // pre:
  //   r0 -- x
  //   r4 -- 4
  //   rbegin_null..r255 -- 0
  // modifies:
  //   r1, r3, r6
  // post:
  //   r6 -- h(x)
  //   rbegin_null..r255 -- 0
  template <bool only_count>
  size_t to_instr_bytes_no_data(std::byte *&data, uint8_t &begin_null) {
    size_t res = 0;

    /* r1 = M */
    res += write_put<only_count>(data, 1, M);

    /* r3 = a */
    res += write_put<only_count>(data, 3, a);
    /* (r6, r5(assumed 0)) = x * a */
    res += write_mul<only_count>(data, 0, 3, 6, begin_null++);

    /* r3 = b */
    res += write_put<only_count>(data, 3, b);
    /* r6 += b */
    res += write_add<only_count>(data, 6, 3, 6);

    /* r3 = p */
    res += write_put<only_count>(data, 3, p);
    /* (r6, r5(assumed 0)) %= p */
    res += write_mod<only_count>(data, 6, 3, 6, begin_null++);
    /* (r6, r5(assumed 0)) %= M */
    res += write_mod<only_count>(data, 6, 1, 6, begin_null++);

    return res;
  }
};

std::default_random_engine hash_function::generator;
std::vector<uint32_t> hash_function::primes = count_primes();

class hash_table_entry {
 private:
  const size_t M;
  std::vector<uint32_t> chain;
  hash_function function;

 public:
  hash_table_entry(const std::vector<std::pair<int, uint32_t>> a)
      : M(a.size() * a.size()), chain(M), function((uint32_t)M) {
    bool run;
    do {
      run = false;
      constexpr uint32_t invalid = std::numeric_limits<uint32_t>::max();
      for (auto &el : chain) el = invalid;
      for (auto el : a) {
        size_t at = function.count(el.first);
        if (chain[at] != invalid) {
          function.recreate((uint32_t)M);
          run = true;
          break;
        }
        chain[at] = el.second;
      }
    } while (run);
  }

  template <bool only_count>
  size_t to_data_bytes(std::byte *&data) {
    size_t res = 0;
    res += write_bytes<only_count>(data, (uint32_t)M);
    res += function.to_data_bytes<only_count>(data);
    res += write_bytes<only_count>(data, chain.data(), M);
    return res;
  }

  // pre:
  //   r0 -- x
  //   r2 -- memory slot
  //   r4 -- 4
  //   rbegin_null..r256 -- 0
  // modifies:
  //   r0, r1, r2, r3, r6
  // post:
  //   r0 -- perfect_hash(x)
  //   r2 -- moved past local data
  //   rbegin_null..r255 -- 0
  template <bool only_count>
  static size_t to_instr_bytes(std::byte *&data, uint8_t &begin_null) {
    size_t res = 0;

    /* r1 = M */
    res += write_load<only_count>(data, 1, 2);
    /* r2 += 4 */
    res += write_add<only_count>(data, 2, 4, 2);

    /* perform hash2 */
    res += hash_function::to_instr_bytes<only_count>(data, begin_null);
    // r6 <-- h(x), r2 <-- chain data address

    /* (r6, r5(assumed 0)) = r6 * 4 */
    res += write_mul<only_count>(data, 6, 4, 6, 5);
    /* r6 = chain data address (r6 = chain.data() + h(x) * 4)*/
    res += write_add<only_count>(data, 2, 6, 6);

    /* load perfect hash */
    res += write_load<only_count>(data, 0, 6);

    return res;
  }
};

class hash_table {
 private:
  const size_t M;
  std::vector<hash_table_entry> entries;
  hash_function function;

 public:
  hash_table(const std::vector<int> &a)
      : M(a.size()), function((uint32_t)M) {
    std::vector<std::vector<std::pair<int, uint32_t>>> numbers(M);

    for (size_t i = 0; i < a.size(); i++)
      numbers[function.count(a[i])].push_back({a[i], (uint32_t)i});
    entries.reserve(M);
    for (size_t i = 0; i < M; i++) entries.emplace_back(numbers[i]);
  }

  template <bool only_count>
  size_t to_data_bytes(std::byte *&data) {
    size_t res = 0;

    uint32_t addr = (uint32_t)(4 * M);
    for (auto &entry : entries) {
      res += write_bytes<only_count>(data, addr);
      addr += (uint32_t)entry.to_data_bytes<true>(data);
    }
    for (auto &entry : entries) res += entry.to_data_bytes<only_count>(data);

    return res;
  }

  // pre:
  //   r0 -- x
  //   r2 -- memory slot
  //   r4 -- 4
  //   rbegin_null..r255 -- 0
  // modifies:
  //   r0, r1, r2, r3, r6
  // post:
  //   r0 -- perfect_hash(x)
  //   rbegin_null..r255 -- 0
  template <bool only_count>
  size_t to_instr_bytes(std::byte *&data, uint8_t &begin_null) {
    size_t res = 0;

    /* perform hash1 */
    res += function.to_instr_bytes_no_data<only_count>(data, begin_null);
    // r6 <-- h(x)

    /* (r6, r5(assumed 0)) = r6 * 4 (r6 = offset to entry data offset)*/
    res += write_mul<only_count>(data, 6, 4, 6, 5);
    /* r6 = address of entry data offset */
    res += write_add<only_count>(data, 2, 6, 6);
    /* r6 = entry data offset */
    res += write_load<only_count>(data, 6, 6);
    /* r2 = entry data address */
    res += write_add<only_count>(data, 6, 2, 2);

    /* submit to entry */
    res += hash_table_entry::to_instr_bytes<only_count>(data, begin_null);
    // r0 <-- perfect hash(x)

    return res;
  }
};

template <bool only_count>
size_t to_instr_bytes_begin(std::byte *&data, uint32_t memory_slot) {
  size_t res = 0;

  res += write_put<only_count>(data, 2, memory_slot);
  res += write_put<only_count>(data, 4, uint32_t{4});

  return res;
}

template <bool only_count>
size_t to_instr_bytes_end(std::byte *&data) {
  size_t res = 0;

  res += write_ret<only_count>(data);

  return res;
}

/*
uint32_t interpret_bytes(std::vector<std::byte> data, uint32_t input) {
  size_t ip = 0;
  std::array<uint32_t, 256> reg{};
  reg[0] = input;
  uint8_t r1, r2, r3, r4;
  uint8_t b0, b1, b2, b3;
  uint64_t res64;

  while (ip < data.size()) {
    switch ((op)data[ip++]) {
      case op::ADD:
        r1 = (uint8_t)data[ip++];
        r2 = (uint8_t)data[ip++];
        r3 = (uint8_t)data[ip++];
        reg[r3] = reg[r1] + reg[r2];
        break;
      case op::MUL:
        r1 = (uint8_t)data[ip++];
        r2 = (uint8_t)data[ip++];
        res64 = (uint64_t)reg[r1] * reg[r2];
        r3 = (uint8_t)data[ip++];
        r4 = (uint8_t)data[ip++];
        reg[r3] = (uint32_t)res64;
        reg[r4] = (uint32_t)(res64 >> 32);
        break;
      case op::LOAD:
        r1 = (uint8_t)data[ip++];
        r2 = (uint8_t)data[ip++];
        reg[r1] = (uint8_t)data[reg[r2]] | ((uint8_t)data[reg[r2] + 1] << 8) |
                  ((uint8_t)data[reg[r2] + 2] << 16) |
                  ((uint8_t)data[reg[r2] + 3] << 24);
        break;
      case op::PUT:
        r1 = (uint8_t)data[ip++];
        b0 = (uint8_t)data[ip++];
        b1 = (uint8_t)data[ip++];
        b2 = (uint8_t)data[ip++];
        b3 = (uint8_t)data[ip++];
        reg[r1] = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
        break;
      case op::MOD:
        r1 = (uint8_t)data[ip++];
        r2 = (uint8_t)data[ip++];
        r3 = (uint8_t)data[ip++];
        r4 = (uint8_t)data[ip++];
        reg[r3] = (uint32_t)((((uint64_t)reg[r4] << 32) | reg[r1]) % reg[r2]);
        break;
      case op::RET:
        return reg[0];
        break;
      default:
        throw std::exception("Unsupported operation code");
    }
  }
  throw std::exception("End of memory reached");
}
*/

int main() {
  int n;
  std::cin >> n;
  std::vector<int> a(n);
  for (int i = 0; i < n; i++) std::cin >> a[i];

  hash_table table = hash_table(a);

  uint8_t begin_null = 7;

  std::byte *ptr = nullptr;
  size_t instr_size = to_instr_bytes_begin<true>(ptr, 0) +
                      table.to_instr_bytes<true>(ptr, begin_null) +
                      to_instr_bytes_end<true>(ptr);
  std::vector<std::byte> data(instr_size + table.to_data_bytes<true>(ptr));

  begin_null = 7;
  ptr = data.data();
  to_instr_bytes_begin<false>(ptr, (uint32_t)instr_size);
  table.to_instr_bytes<false>(ptr, begin_null);
  to_instr_bytes_end<false>(ptr);
  table.to_data_bytes<false>(ptr);

  std::cout << std::hex << std::setfill('0');
  for (auto el : data) std::cout << std::setw(2) << (int)el;
  std::cout << std::endl;

  return 0;
}
