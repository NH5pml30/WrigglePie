/* Kholiavin Nikolai, M3138 */

#include <iostream>
#include <vector>
#include <chrono>

long long CountInvsRec(std::vector<int> &A, size_t L, size_t R) {
  if (R - L == 1)
    return 0;

  long long res = CountInvsRec(A, L, (R + L) / 2) + CountInvsRec(A, (R + L) / 2, R);

  std::vector<int> buffer(R - L);
  memcpy(buffer.data(), &A[L], sizeof(int) * (R - L));

  auto
    p1 = buffer.cbegin(),
    end1 = p1 + (R - L) / 2,
    p2 = end1,
    end2 = buffer.cend();

  size_t cur = 0;
  while (cur < buffer.size())
    if (p1 == end1 || (p2 != end2 && *p2 < *p1))
      res += end1 - p1, A[L + cur++] = *p2++;
    else
      res += p2 - end1, A[L + cur++] = *p1++;
  return res;
}

long long CountInvs(std::vector<int> &A) {
  return CountInvsRec(A, 0, A.size()) / 2;
}

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
};

int main() {
  int n, m;
  unsigned a, b;
  std::cin >> n >> m >> a >> b;
  std::vector<int> arr(n);
  generator random(a, b);

  for (int i = 0; i < n; i++)
    arr[i] = random.NextRand24() % m;
  std::cout << CountInvs(arr) << std::endl;

  return 0;
}
