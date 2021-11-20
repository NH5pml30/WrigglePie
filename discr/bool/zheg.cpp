/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <bitset>
#include <string>

int main()
{
  int n;
  std::cin >> n;
  std::vector<bool> table(1 << n);

  std::string dummy;
  for (int i = 0; i < (1 << n); i++)
  {
    bool x;
    std::cin >> dummy >> x;
    table[i] = x;
  }

  for (int i = 0; i < n; i++)
    std::cout << '0';
  std::cout << ' ' << table[0] << std::endl;

  std::vector<bool> v2 = table;
  std::vector<bool> *front = &table, *back = &v2;

  for (int i = 1; i < table.size(); i++)
  {
    for (int j = 0; j < table.size() - i; j++)
      (*back)[j] = (*front)[j] ^ (*front)[j + 1];
    for (int j = 0; j < n; j++)
      std::cout << ((i >> (n - j - 1)) & 1);
    std::cout << ' ' << (*back)[0] << std::endl;
    std::swap(front, back);
  }

  return 0;
}
