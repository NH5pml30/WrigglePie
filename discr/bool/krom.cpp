/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>

class orgraph
{
private:
  std::vector<bool> Matrix;
  int N;

  bool IsReachable( int From, int To, std::vector<bool> &Was )
  {
    if (From == To)
      return true;

    Was[From] = true;
    for (int i = 0; i < N; i++)
      if (Matrix[From * N + i] && !Was[i] && IsReachable(i, To, Was))
        return true;
    return false;
  }

public:
  orgraph( int N ) : Matrix(N * N), N(N)
  {
  }

  void AddEdge( int From, int To )
  {
    Matrix[From * N + To] = 1;
  }

  bool IsReachable( int From, int To )
  {
    std::vector<bool> was(N);
    return IsReachable(From, To, was);
  }
};

int main() {
  int n, m;
  std::cin >> n >> m;

  orgraph g(2 * n);

  auto trans = [&]( int A )
  {
    if (A > 0)
      return A - 1;
    return -A - 1 + n;
  };

  auto negate = [&]( int A )
  {
    return A >= n ? A - n : A + n;
  };

  for (int i = 0; i < m; i++)
  {
    int a, b;
    std::cin >> a >> b;

    a = trans(a);
    b = trans(b);

    g.AddEdge(negate(a), b);
    g.AddEdge(negate(b), a);
  }

  for (int i = 0; i < n; i++)
    if (g.IsReachable(i, negate(i)) && g.IsReachable(negate(i), i))
    {
      std::cout << "YES" << std::endl;
      return 0;
    }

  std::cout << "NO" << std::endl;
  return 0;
}
