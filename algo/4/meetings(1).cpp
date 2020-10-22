/* Nikolai Kholiavin, M3138 */
#include <fstream>
#include <vector>
#include <algorithm>
#include <string>
#include <bitset>

struct MEETING {
  int min, max, delta;

  MEETING(int min, int max, int delta) : min(min), max(max), delta(delta) {
  }

  MEETING() {
  }
};

std::istream & operator>>(std::istream &O, MEETING &Meeting) {
  return O >> Meeting.min >> Meeting.max >> Meeting.delta;
}

int GetLastBit(int X) {
  int res;
  for (res = sizeof(X) * 8 - 1; res >= 0; res--)
    if ((X >> res) > 0)
      break;
  return res;
}

int SumBits(int X) {
  int res = 0;
  while (X > 0) {
    res += X & 1;
    X >>= 1;
  }
  return res;
}

int main() {
  std::string name = "meetings";
  std::ifstream in(name + ".in");
  std::ofstream out(name + ".out");

  int n, k;
  in >> n >> k;

  std::vector<MEETING> data(n);
  for (int i = 0; i < n; i++) {
    in >> data[i];
  }

  int pown = 1 << n;

  std::vector<int> sum(pown, k);
  for (int A = 1; A < pown; A++) {
    int last_bit = GetLastBit(A);
    sum[A] = sum[A ^ (1 << last_bit)] + data[last_bit].delta;
  }

  std::vector<std::bitset<20>> fb(pown);
  std::vector<int> fi(pown * n);

  auto get = [&](int A, int i) -> int & {
    return fi[A * n + i];
  };

  std::vector<int> ans;
  for (int start = 0; start < n; start++)
    if (k >= data[start].min && k <= data[start].max) {
      fb[1 << start].reset();
      fb[1 << start].set(start);

      int maxA = 1 << start, max_bits = 1, last = start;

      for (int A = 0; A < pown; A++) {
        if (((A >> start) & 1) && A != (1 << start)) {
          fb[A].reset();
          int bits = SumBits(A);

          for (int i = 0; i < n; i++)
            if (i != start && (A >> i) & 1) {
              int Awo = A ^ (1 << i);
              if (sum[Awo] >= data[i].min && sum[Awo] <= data[i].max) {
                for (int j = 0; j < n; j++)
                  if (fb[Awo].test(j)) {
                    get(A, i) = j;
                    fb[A].set(i);
                    if (bits > max_bits) {
                      maxA = A;
                      max_bits = bits;
                      last = i;
                    }
                  }
              }
            }
        }
      }

      if (max_bits > (int)ans.size()) {
        ans.resize(max_bits);
        for (int i = max_bits - 1; i >= 0; i--) {
          ans[i] = last;
          last = get(maxA, last);
          maxA ^= 1 << ans[i];
        }
      }
    }

  out << ans.size() << std::endl;
  for (auto el : ans)
    out << el + 1 << ' ';
  out << std::endl;

  return 0;
}
