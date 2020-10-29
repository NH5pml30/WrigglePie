/* Kholiavin Nikolai, M3138 */
#include <iostream>
#include <vector>
#include <functional>

template<typename type>
  void Merge(std::vector<type *> &A, int L, int Half, int R,
             std::function<bool(const type &Left, const type &Right)> Less) {
    std::vector<type *> buffer(R - L);
    memcpy(buffer.data(), &A[L], sizeof(type *) * (R - L));

    auto
      p1 = buffer.cbegin(),
      end1 = p1 + (Half - L),
      p2 = end1,
      end2 = buffer.cend();

    size_t cur = 0;
    while (cur < buffer.size())
      if (p1 == end1 || (p2 != end2 && Less(**p2, **p1)))
        A[L + cur++] = *p2++;
      else
        A[L + cur++] = *p1++;
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

  unsigned int NextRand32() {
    unsigned int
      a = NextRand24(),
      b = NextRand24();
    return (a << 8) ^ b;
  }
};


struct SUM_DATA {
  long long Sum;
  SUM_DATA *Prev = nullptr, *Next = nullptr;
  int Index;

  SUM_DATA() {
  }

  SUM_DATA(long long Sum, SUM_DATA *Prev, SUM_DATA *Next, int Index) :
    Sum(Sum), Prev(Prev), Next(Next), Index(Index) {
  }

  bool LessCur(const SUM_DATA &Other) const {
    return Sum < Other.Sum;
  }

  bool LessLast(const SUM_DATA &Other) const {
    return (Prev == nullptr ? 0 : Prev->Sum) < (Other.Prev == nullptr ? 0 : Other.Prev->Sum);
  }

  long long operator-(const SUM_DATA &Other) const {
    return Sum - (Other.Prev == nullptr ? 0 : Other.Prev->Sum);
  }
};

long long SumGreaterK(std::vector<SUM_DATA *> &A, int Left, int Right,
                      long long K, bool IsLast = false) {
  if (Right == Left)
    return 0;
  if (Right - Left == 1)
    return *A[Left] - *A[Left] >= K;

  int half = (Left + Right) / 2;
  long long res = 0;

  // sort left by prev & count
  res += SumGreaterK(A, Left, half, K, true);
  // sort right by this & count
  res += SumGreaterK(A, half, Right, K, false);

  // count left,right pairs
  auto
    p1 = A.cbegin() + Left, end1 = A.cbegin() + half,
    p2 = end1, end2 = A.cbegin() + Right;

  while (p1 != end1 && p2 != end2) {
    if (**p2 - **p1 >= K) {
      res += end2 - p2;
      p1++;
    } else {
      p2++;
    }
  }

  SUM_DATA *head = nullptr;
  int tail_index = -1;
  if (IsLast) {
    // shift right so that it is sorted by prev, not this (this = next)
    for (int i = half; i < Right; i++) {
      if (A[i]->Index == half)
        head = A[i];
      if (A[i]->Index == Right - 1)
        tail_index = i;
      else
        A[i] = A[i]->Next;
    }
    // insert last element into sorted array
    A[tail_index] = head;
    if (tail_index > half && A[tail_index]->LessLast(*A[tail_index - 1])) {
      while (tail_index > half && A[tail_index]->LessLast(*A[tail_index - 1])) {
        std::swap(A[tail_index - 1], A[tail_index]);
        tail_index--;
      }
    } else {
      while (tail_index < Right - 1 && A[tail_index + 1]->LessLast(*A[tail_index])) {
        std::swap(A[tail_index + 1], A[tail_index]);
        tail_index++;
      }
    }
  } else {
    // shift left so that it is sorted by this, not prev (this = prev)
    for (int i = Left; i < half; i++) {
      if (A[i]->Index == half - 1)
        head = A[i];
      if (A[i]->Index == Left)
        tail_index = i;
      else
        A[i] = A[i]->Prev;
    }
    // insert first element into sorted array
    A[tail_index] = head;
    if (tail_index > Left && A[tail_index]->LessCur(*A[tail_index - 1])) {
      while (tail_index > Left && A[tail_index]->LessCur(*A[tail_index - 1])) {
        std::swap(A[tail_index - 1], A[tail_index]);
        tail_index--;
      }
    } else {
      while (tail_index < half - 1 && A[tail_index + 1]->LessCur(*A[tail_index])) {
        std::swap(A[tail_index + 1], A[tail_index]);
        tail_index++;
      }
    }
  }

  Merge(A, Left, half, Right, std::function<bool(const SUM_DATA &Left, const SUM_DATA &Right)>(
    IsLast ? &SUM_DATA::LessLast : &SUM_DATA::LessCur));

  return res;
}

long long Check(const std::vector<SUM_DATA *> &S, long long K) {
  long long res = 0;
  for (int i = 0; i < (int)S.size(); i++)
    for (int j = i; j < (int)S.size(); j++)
      res += (*S[j] - *S[i] >= K);
  return res;
}

int main() {
  int n;
  long long k;
  unsigned a, b;
  std::cin >> n >> k >> a >> b;
  generator gen(a, b);

  std::vector<SUM_DATA *> sum(n);
  sum[0] = new SUM_DATA((int)gen.NextRand32(), nullptr, nullptr, 0);
  for (int i = 1; i < n; i++)
    sum[i] = new SUM_DATA(sum[i - 1]->Sum + (int)gen.NextRand32(),
                          sum[i - 1], nullptr, i);
  for (int i = 0; i < n - 1; i++)
    sum[i]->Next = sum[i + 1];

  // std::cout << Check(sum, k) << ":";
  std::cout << SumGreaterK(sum, 0, n, k) << std::endl;
  return 0;
}
