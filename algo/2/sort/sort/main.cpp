/* Kholiavin Nikolai, M3138 */

#include <iostream>
#include <vector>
#include <chrono>

void MergeSortRec(std::vector<int> &A, size_t L, size_t R) {
  if (R - L == 1)
    return;

  MergeSortRec(A, L, (R + L) / 2);
  MergeSortRec(A, (R + L) / 2, R);

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
      A[L + cur++] = *p2++;
    else
      A[L + cur++] = *p1++;
}

void MergeSort(std::vector<int> &A) {
  MergeSortRec(A, 0, A.size());
}

int main() {
  int n;
  std::cin >> n;
  std::vector<int> a(n);

  for (int i = 0; i < n; i++)
    std::cin >> a[i];
  MergeSort(a);

  for (int i = 0; i < n; i++)
    std::cout << a[i] << " ";
  std::cout << std::endl;

  return 0;
}
