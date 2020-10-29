#include <iostream>
#include <vector>
#include <cstring>

using vector = std::vector<unsigned int>;

class generator {
 private:
  unsigned int
    Cur = 0,
    A, B;

 public:
  generator(unsigned int A, unsigned int B) : A(A), B(B) {
  }

  unsigned int NextRand24() {
    Cur = Cur * A + B;
    return Cur >> 8;
  }

  unsigned int NextRand32() {
    unsigned int
      a = NextRand24(),
      b = NextRand24();
    return (a << 8) ^ b;
  }
};

int main() {
  int t, n;
  std::cin >> t >> n;

  unsigned int a, b;
  std::cin >> a >> b;
  generator gen(a, b);

  constexpr unsigned int iter = 4;
  int counts[iter][1 << (sizeof(unsigned int) * 8 / iter)];
  vector a1(n), a2(n), *front = &a1, *back = &a2;

  for (int i = 0; i < t; i++) {
    memset(counts, 0, sizeof(counts));
    for (int j = 0; j < n; j++) {
      (*front)[j] = gen.NextRand32();

      for (unsigned k = 0; k < iter; k++)
        counts[k][((*front)[j] >> (k * 8)) & 0xFF]++;
    }

    for (unsigned k = 0; k < iter; k++) {
      int glob_count = 0;
      for (unsigned j = 0; j < sizeof(counts[k]) / sizeof(counts[k][0]); j++) {
        int save = counts[k][j];
        counts[k][j] = glob_count;
        glob_count += save;
      }

      for (int j = 0; j < n; j++)
        (*back)[counts[k][((*front)[j] >> (k * 8)) & 0xFF]++] = (*front)[j];

      std::swap(front, back);
    }

    unsigned long long sum = 0;
    for (int j = 0; j < n; j++)
      sum += (*front)[j] * (j + 1ull);
    std::cout << sum << std::endl;
  }

  return 0;
}
