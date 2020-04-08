/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <list>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>

int main() {
  // same hash 2135687
  std::string same_hash[] = {
    "Doea",
    "EQGB",
    "EQFa",
    "EPea",
    "DofB",
    "DpFa",
    "DpGB",
    "EPfB",
  };
  // hash 0
  std::string zero_hash = "zeZnzeZp";

  int n;
  std::cin >> n;

  for (int i = 0; i < n; i++) {
    for (auto str : same_hash) {
      for (int j = 0; j < 125 && i < n; j++, i++, str = zero_hash + str)
        std::cout << str << std::endl;
    }
  }

  return 0;
}
