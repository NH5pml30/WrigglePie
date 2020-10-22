/* Nikolai Kholiavin, M3138 */
#include <fstream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cstring>

std::vector<int> Set;
std::vector<bool> Lookup;
std::vector<long long> F;
int PowN, N;

long long & Get(int Subset, int Start) {
  return F[Subset * N + Start];
}

void Precalc(int k) {
  int n = Set.size();
  Lookup.resize(n * n);

  N = n;
  PowN = 1 << n;
  F.resize(PowN * n, 0);

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      Lookup[i * n + j] = Lookup[j * n + i] = std::gcd(Set[i], Set[j]) >= k;

  std::vector<long long> local_f(PowN * n);
  auto get = [&](int Subset, int Last) -> long long & {
    return local_f[Subset * n + Last];
  };

  for (int start = 0; start < n; start++) {
    memset(local_f.data(), 0, sizeof(long long) * PowN * n);
    get(1 << start, start) = 1;
    Get(1 << start, start) = 1;

    for (int sub = 0; sub < PowN; sub++)
      if (((sub >> start) & 1) && sub != (1 << start)) {
        for (int last = 0; last < n; last++)
          if (last != start && ((sub >> last) & 1)) {
            int subwo = sub ^ (1 << last);
            for (int prelast = 0; prelast < n; prelast++)
              if (((subwo >> prelast) & 1) && Lookup[last * n + prelast]) {
                get(sub, last) += get(subwo, prelast);
                Get(sub, start) += get(subwo, prelast);
              }
          }
      }
  }
}

void Gen(std::vector<int> &Cur, int Subset, int k, std::ostream &out) {
  if ((int)Cur.size() == N) {
    for (int i = 0; i < N; i++)
      out << Set[Cur[i]] << ' ';
    out << std::endl;
    return;
  }

  int last = Cur.size() > 0 ? Cur.back() : -1;
  Cur.push_back(0);
  for (int i = 0; i < N; i++)
    if (!((Subset >> i) & 1) && (last == -1 || Lookup[last * N + i])) {
      long long cnt = Get((PowN - 1) ^ Subset, i);
      if (cnt <= k) {
        k -= (int)cnt;
      } else {
        Cur.back() = i;
        Gen(Cur, Subset | (1 << i), k, out);
        return;
      }
    }
  Cur.pop_back();
  out << "-1" << std::endl;
}

int main() {
  std::string name = "perm";
  std::ifstream in(name + ".in");
  std::ofstream out(name + ".out");

  int n, m, k;
  in >> n >> m >> k;

  Set.resize(n);
  for (int i = 0; i < n; i++)
    in >> Set[i];

  std::sort(Set.begin(), Set.end());

  Precalc(k);

  std::vector<int> dummy;
  Gen(dummy, 0, m - 1, out);

  return 0;
}
