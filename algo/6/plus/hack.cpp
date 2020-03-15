/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>

int main() {
  std::vector<uint32_t> coefs(1000);
  uint32_t mult = 1;
  for (int i = 0; i < 1000; i++) {
    coefs[i] = mult;
    mult *= 31;
  }

  std::vector<int> hashes((size_t)1 << 16);
  for (int a = 'a'; a < 'z'; a++) hashes[a]++;
  for (int a = 'A'; a < 'Z'; a++) hashes[a]++;
  int i;
  int max = 0, maxi = -1;
  for (i = 0; i < 1000; i++) {
    auto newhashes = hashes;
    for (size_t i = 0; i < hashes.size(); i++) {
      max = max > hashes[i] ? max : (maxi = i, hashes[i]);
      for (int a = 'a'; a < 'z'; a++)
        if (uint32_t ind = (uint32_t)i * 31 + a; ind < (1 << 16))
          newhashes[ind] += hashes[i];
      for (int a = 'A'; a < 'Z'; a++)
        if (uint32_t ind = (uint32_t)i * 31 + a; ind < (1 << 16))
          newhashes[ind] += hashes[i];

      newhashes.swap(hashes);
    }

    std::cout << max << std::endl;
    if (max > 1000) break;
  }

  std::cout << i << ' ';
  for (size_t i = 0; i < hashes.size(); i++)
    if (hashes[i] >= 1000) {
      std::cout << i << std::endl;
      break;
    }

  std::cout << (int)'a' << ' ' << (int)'z' << ' ' << (int)'A' << ' ' << (int)'Z'
            << std::endl;

  return 0;
}
