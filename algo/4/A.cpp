/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <fstream>

using namespace std;

void OutputAnswer(ofstream &out, const vector<vector<pair<int, int>>> f, int i, int j) {
  if (i == j) {
    out << "A";
  } else {
    out << "(";
    OutputAnswer(out, f, i, f[i][j].second);
    OutputAnswer(out, f, f[i][j].second + 1, j);
    out << ")";
  }
}

int main() {
  ifstream in("matrix.in");
  ofstream out("matrix.out");
  int n;
  in >> n;
  vector<vector<pair<int, int>>> f(n);
  vector<pair<int, int>> dims(n);

  constexpr int inf = numeric_limits<int>::max();

  in >> dims[0].first >> dims[0].second;
  for (int i = 1; i < n; i++) {
    f[i - 1].resize(n, {inf, -1});
    f[i - 1][i - 1] = {0, -1};

    in >> dims[i].first >> dims[i].second;
    f[i - 1][i] = {dims[i - 1].first * dims[i].first * dims[i].second, i - 1};
  }
  f[n - 1].resize(n, {inf, -1});
  f[n - 1][n - 1] = {0, -1};

  for (int i = n - 2; i >= 0; i--)
    for (int j = i + 2; j < n; j++)
      for (int k = i; k < j; k++)
        if (int val = f[i][k].first + f[k + 1][j].first +
              dims[i].first * dims[k].second * dims[j].second;
            val < f[i][j].first)
          f[i][j] = {val, k};

  OutputAnswer(out, f, 0, n - 1);
  return 0;
}
