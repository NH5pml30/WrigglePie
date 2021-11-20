/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>

class node
{
private:
  std::vector<node *> Inputs;
  std::vector<bool> TruthTable;
  int Var = 0;
  int Depth = 0;

public:
  node( std::vector<node *> &&Inputs, std::vector<bool> &&TruthTable ) :
    Inputs(Inputs), TruthTable(TruthTable)
  {
    for (auto el : Inputs)
      Depth = std::max(Depth, el->Depth + 1);
  }

  node( int Var ) : Var(Var), Depth(0)
  {
  }

  int GetDepth()
  {
    return Depth;
  }

  bool Count( const std::vector<bool> &Vars )
  {
    if (Inputs.size() == 0)
      return Vars[Var];
    int args = 0;
    for (int i = 0; i < Inputs.size(); i++)
      args |= Inputs[i]->Count(Vars) << (Inputs.size() - i - 1);
    return TruthTable[args];
  }
};

int main()
{
  int n;
  std::cin >> n;

  std::vector<node *> scheme(n);

  int k = 0;
  for (int i = 0; i < n; i++)
  {
    int m;
    std::cin >> m;

    if (m == 0)
      scheme[i] = new node(k++);
    else
    {
      std::vector<node *> inputs(m);
      for (int j = 0; j < m; j++)
      {
        int ind;
        std::cin >> ind;
        inputs[j] = scheme[ind - 1];
      }

      std::vector<bool> table(1 << m);
      for (int i = 0; i < (1 << m); i++)
      {
        bool x;
        std::cin >> x;
        table[i] = x;
      }

      scheme[i] = new node(std::move(inputs), std::move(table));
    }
  }

  std::cout << scheme.back()->GetDepth() << std::endl;

  std::vector<bool> vars(k);
  for (int i = 0; i < (1 << k); i++)
  {
    for (int j = 0; j < k; j++)
      vars[k - j - 1] = (i >> j) & 1;
    std::cout << scheme.back()->Count(vars);
  }
  std::cout << std::endl;

  return 0;
}
