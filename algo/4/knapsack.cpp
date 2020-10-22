/* Nikolai Kholiavin, M3138 */
#include <fstream>
#include <vector>
#include <algorithm>

std::vector<int> Calc(int L, int R, int W,
                      const std::vector<int> &Weights,
                      const std::vector<int> &Costs) {
  std::vector<int> f(W + 1, std::numeric_limits<int>::min());
  f[0] = 0;

  for (int i = L; i < R; i++) {
    for (int k = W; k >= Weights[i]; k--)
      f[k] = std::max(f[k], f[k - Weights[i]] + Costs[i]);
  }

  return f;
}

void Get(int L, int R, int W, int F,
         const std::vector<int> &Weights,
         const std::vector<int> &Costs,
         std::vector<int> &ans) {
  if (L == R - 1) {
    if (W > 0)
      ans.push_back(L);
    return;
  }

  int M = (L + R) / 2;
  std::vector<int>
    f1 = Calc(L, M, W, Weights, Costs),
    f2 = Calc(M, R, W, Weights, Costs);
  for (int k1 = 0; k1 <= W; k1++) {
    int k2 = W - k1;
    if (int left = f1[k1], right = f2[k2]; left + right == F) {
      f1.clear();
      f2.clear();
      Get(L, M, k1,  left, Weights, Costs, ans);
      Get(M, R, k2, right, Weights, Costs, ans);
      return;
    }
  }
}

int main() {
  std::string name = "knapsack";
  std::ifstream in(name + ".in");
  std::ofstream out(name + ".out");

  int n, m;
  in >> n >> m;

  std::vector<int> w(n), c(n);

  for (int i = 0; i < n; i++)
    in >> w[i];
  for (int i = 0; i < n; i++)
    in >> c[i];

  std::vector<int> f = Calc(0, n, m, w, c);
  int max_cost = f[0], all_weight = 0;
  for (int i = 0; i <= m; i++)
    if (f[i] > max_cost) {
      max_cost = f[i];
      all_weight = i;
    }
  f.clear();

  Get(0, n, all_weight, max_cost, w, c, f);
  out << f.size() << std::endl;
  for (int el : f)
    out << el + 1 << ' ';
  out << std::endl;
  return 0;
}
