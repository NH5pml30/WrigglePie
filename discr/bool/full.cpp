/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>

bool CheckNotSave0( const std::vector<bool> &TruthTable )
{
  return TruthTable[0];
}

bool CheckNotSave1( const std::vector<bool> &TruthTable )
{
  return !TruthTable[TruthTable.size() - 1];
}

bool CheckNotSelfdual( const std::vector<bool> &TruthTable )
{
  if (TruthTable.size() == 1)
    return true;

  for (int i = 0; i < TruthTable.size() / 2; i++)
    if (TruthTable[i] == TruthTable[TruthTable.size() - i - 1])
      return true;
  return false;
}

bool CheckNotMonotonic( const std::vector<bool> &TruthTable )
{
  for (int i = 0; i < TruthTable.size(); i++)
    if (TruthTable[i])
      for (int j = i + 1; j < TruthTable.size(); j++)
        if (((j | (~i)) & (TruthTable.size() - 1)) == TruthTable.size() - 1 && !TruthTable[j])
          return true;
  return false;
}

bool IsntOneBit( int N )
{
  int res = 0;
  for (; N > 0; N >>= 1)
    res += N & 1;
  return res != 1;
}

bool CheckNotLinear( const std::vector<bool> &TruthTable )
{
  std::vector<bool> v1 = TruthTable, v2 = TruthTable;
  std::vector<bool> *front = &v1, *back = &v2;

  for (int i = 1; i < TruthTable.size(); i++)
  {
    for (int j = 0; j < TruthTable.size() - i; j++)
      (*back)[j] = (*front)[j] ^ (*front)[j + 1];
    if (IsntOneBit(i) && (*back)[0])
      return true;
    std::swap(front, back);
  }
  return false;
}

int main()
{
  int n;
  std::cin >> n;

  bool classes[5] = {};
  bool (*checks[5])(const std::vector<bool> &) =
  {
    CheckNotSave0,
    CheckNotSave1,
    CheckNotSelfdual,
    CheckNotMonotonic,
    CheckNotLinear
  };

  for (int i = 0; i < n; i++)
  {
    int s;
    std::cin >> s;

    std::vector<bool> table(1 << s);
    for (int i = 0; i < (1 << s); i++)
    {
      char x;
      std::cin >> x;
      table[i] = x - '0';
    }

    for (int i = 0; i < 5; i++)
      if (!classes[i])
        classes[i] = checks[i](table);

    bool is_end = true;
    for (int i = 0; i < 5; i++)
      if (!classes[i])
        is_end = false;
    if (is_end)
    {
      std::cout << "YES" << std::endl;
      return 0;
    }
  }

  std::cout << "NO" << std::endl;
  return 0;
}
