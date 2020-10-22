/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

std::vector<int> Calc(const std::string &A, int La, int Ra,
                      const std::string &B, int Lb, int Rb, bool IsReversed) {
  std::vector<int> f[2] {
    std::vector<int>(Rb - Lb + 1),
    std::vector<int>(Rb - Lb + 1)
  };

  auto geta = [&](int i) {
    return A[IsReversed ? Ra - i : La + i - 1];
  };
  auto getb = [&](int i) {
    return B[IsReversed ? Rb - i : Lb + i - 1];
  };

  for (int i = 1; i <= Ra - La; i++) {
    f[1][0] = 0;
    for (int j = 1; j <= Rb - Lb; j++) {
      f[0][j] = f[1][j];
      if (geta(i) == getb(j))
        f[1][j] = f[0][j - 1] + 1;
      else
        f[1][j] = std::max(f[0][j], f[1][j - 1]);
    }
  }

  return f[1];
}

void Get(const std::string &A, int La, int Ra,
         const std::string &B, int Lb, int Rb, int Len) {
  if (La == Ra || Lb == Rb)
    return;
  if (La == Ra - 1) {
    if (Len > 0)
      std::cout << A[La];
    return;
  }
  if (Lb == Rb - 1) {
    if (Len > 0)
      std::cout << B[Lb];
    return;
  }

  int Ma = (La + Ra) / 2;
  auto
    f1 = Calc(A, La, Ma, B, Lb, Rb, false),
    f2 = Calc(A, Ma, Ra, B, Lb, Rb, true);

  for (int k = 0; k <= Rb - Lb; k++) {
    if (f1[k] + f2[Rb - Lb - k] == Len) {
      int len_left = f1[k], len_right = f2[Rb - Lb - k];
      f1.clear(), f2.clear();
      Get(A, La, Ma, B, Lb, Lb + k, len_left);
      Get(A, Ma, Ra, B, Lb + k, Rb, len_right);
      return;
    }
  }
}

int main() {
  std::string a, b;
  std::cin >> a >> b;

  Get(a, 0, a.size(), b, 0, b.size(),
    Calc(a, 0, a.size(), b, 0, b.size(), false)[b.size()]);

  return 0;
}
