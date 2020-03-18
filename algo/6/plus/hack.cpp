/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <list>
#include <string>
#include <vector>

std::vector<uint32_t> coefs(1000);

std::vector<std::string> dynamic(int l, int r, uint32_t hash) {
  std::vector<std::string> result;
  if (l == r - 1) {
    for (char code = 'a'; code <= 'z'; code++)
      if (code * coefs[l] == hash) result.push_back({code});
    for (char code = 'A'; code <= 'Z'; code++)
      if (code * coefs[l] == hash) result.push_back({code});
    return result;
  }

  int m = (l + r) / 2;
  for (size_t hashl = 0; hashl <= hash; hashl++) {
    uint32_t hash_left = (uint32_t)hashl, hash_right = hash - hash_left;
    auto left = dynamic(l, m, hash_left), right = dynamic(m, r, hash_right);
    for (auto &left_str : left)
      for (auto &right_str : right) result.push_back(left_str + right_str);
  }
  return result;
}

size_t dynamic_count(int l, int r, uint32_t hash) {
  size_t result = 0;

  if (l == r - 1) {
    for (char code = 'a'; code <= 'z'; code++)
      result += code * coefs[l] == hash;
    for (char code = 'A'; code <= 'Z'; code++)
      result += code * coefs[l] == hash;

    return result;
  }

  int m = (l + r) / 2;
  for (size_t hashl = 0; hashl <= ((size_t)1 << 32); hashl++) {
    uint32_t hash_left = (uint32_t)hashl, hash_right = hash - hash_left;
    result += dynamic_count(l, m, hash_left) * dynamic_count(m, r, hash_right);
  }
  return result;
}

std::pair<int, int> count_max(const std::vector<int> &a) {
  if (a.size() == 0) return {std::numeric_limits<int>::min(), -1};
  int result = a[0], ind = 0;
  int i = 0;
  for (auto &el : a) {
    if (result < el) result = el, ind = i;
    i++;
  }
  return {result, ind};
}

int main() {
  uint32_t mult = 1;
  for (int i = 0; i < 1000; i++) {
    coefs[i] = mult;
    mult *= 31;
  }

  std::vector<int> hashes((size_t)1 << 32);
  for (int a = 'a'; a < 'z'; a++) hashes[a]++;
  for (int a = 'A'; a < 'Z'; a++) hashes[a]++;
  int i;
  for (i = 0; i < 1000; i++) {
    auto newhashes = hashes;
    for (size_t i = 0; i < hashes.size(); i++) {
      for (int a = 'a'; a < 'z'; a++)
        newhashes[(uint32_t)i * 31 + a] += hashes[i];
      for (int a = 'A'; a < 'Z'; a++)
        newhashes[(uint32_t)i * 31 + a] += hashes[i];

      newhashes.swap(hashes);
    }

    auto pp = count_max(hashes);
    std::cout << i << ' ' << pp.first << ' ' << pp.second << std::endl;
  }


//  std::cout << dynamic_count(0, 45, 64576) << std::endl;

  return 0;
}
