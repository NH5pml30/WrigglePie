/* Nikolai Kholiavin, M3138 */

#include <iostream>
#include <vector>
#include <algorithm>

int & minimum(int &a1, int &a2) {
  return a1 <= a2 ? a1 : a2;
}

int & minimum(int &a1, int &a2, int &a3) {
  if (a1 <= a2 && a1 <= a3)
    return a1;
  if (a2 <= a3)
    return a2;
  return a3;
}

int & minimum(int &a1, int &a2, int &a3, int &a4) {
  return minimum(minimum(a1, a2, a3), a4);
}

int & minimum(int &a1, int &a2, int &a3, int &a4, int &a5) {
  return minimum(minimum(a1, a2, a3), a4, a5);
}

int median(int a1, int a2, int a3, int a4, int a5) {
  std::swap(minimum(a1, a2, a3, a4, a5), a1);
  std::swap(minimum(a2, a3, a4, a5), a2);
  return minimum(a3, a4, a5);
}

int median(int a1, int a2, int a3, int a4) {
  std::swap(minimum(a1, a2, a3, a4), a1);
  std::swap(minimum(a2, a3, a4), a2);
  return std::min(a3, a4);
}

int median(int a1, int a2, int a3) {
  std::swap(minimum(a1, a2, a3), a1);
  return std::min(a2, a3);
}

int kth(int L, int R, std::vector<int> &a, int k);

std::pair<int, int> partition(int L, int R, std::vector<int> &a) {
  if (R - L == 1) {
    return {L - 1, R};
  } else if (R - L == 2) {
    if (a[L] > a[L + 1])
      std::swap(a[L], a[L + 1]);
    return {L - (a[L] == a[L + 1]), R};
  } else if (R - L == 3) {
    std::swap(minimum(a[L], a[L + 1], a[L + 2]), a[L]);
    if (a[L + 1] > a[L + 2])
      std::swap(a[L + 1], a[L + 2]);
    return {L - (a[L + 1] == a[L]), R - 1 + (a[L + 1] == a[L + 2])};
  } else if (R - L == 4) {
    std::swap(minimum(a[L], a[L + 1], a[L + 2], a[L + 3]), a[L]);
    std::swap(minimum(a[L + 1], a[L + 2], a[L + 3]), a[L + 1]);
    if (a[L + 2] > a[L + 3])
      std::swap(a[L + 2], a[L + 3]);
    return {L + 1 - (a[L + 2] == a[L + 1]) - (a[L + 2] == a[L + 1] && a[L + 2] == a[L]),
            R - 1 + (a[L + 2] == a[L + 3])};
  } else if (R - L == 5) {
    std::swap(minimum(a[L], a[L + 1], a[L + 2], a[L + 3], a[L + 4]), a[L]);
    std::swap(minimum(a[L + 1], a[L + 2], a[L + 3], a[L + 4]), a[L + 1]);
    std::swap(minimum(a[L + 2], a[L + 3], a[L + 4]), a[L + 2]);
    if (a[L + 3] > a[L + 4])
      std::swap(a[L + 3], a[L + 4]);
    return {L + 1 - (a[L + 2] == a[L + 1]) - (a[L + 2] == a[L + 1] && a[L + 2] == a[L]),
            R - 2 + (a[L + 2] == a[L + 3]) + (a[L + 2] == a[L + 3] && a[L + 2] == a[L + 4])};
  }

  /*
  std::vector<int> c((R - L + 4) / 5);

  for (int i = 0; i < (int)c.size(); i++)
    if (i * 5 + 4 < R)
      c[i] = median(a[i * 5], a[i * 5 + 1], a[i * 5 + 2], a[i * 5 + 3], a[i * 5 + 4]);
    else if (i * 5 + 3 < R)
      c[i] = median(a[i * 5], a[i * 5 + 1], a[i * 5 + 2], a[i * 5 + 3]);
    else if (i * 5 + 2 < R)
      c[i] = median(a[i * 5], a[i * 5 + 1], a[i * 5 + 2]);
    else if (i * 5 + 1 < R)
      c[i] = std::max(a[i * 5], a[i * 5 + 1]);
    else
      c[i] = a[i * 5];

  int x = kth(0, (int)c.size(), c, (int)c.size() / 2 + 1);
  */
  int x = a[L + rand() % (R - L)];

  /*
  int b = L - 1, e = R;

  while (true) {
    do {
      b++;
    } while (b < e && a[b] <= x);
    do {
      e--;
    } while (a[e] > x);

    if (b >= e)
      return e;
    std::swap(a[b], a[e]);
  }
  */
  int b = L, e = R - 1, beq = L - 1, eeq = R;
  while (true) {
    while (b <= e && a[b] <= x) {
      if (a[b] == x)
        std::swap(a[++beq], a[b]);
      b++;
    }

    while (e >= b && a[e] >= x) {
      if (a[e] == x)
        std::swap(a[--eeq], a[e]);
      e--;
    }

    if (b > e)
      break;
    std::swap(a[b++], a[e--]);
  }
  for (int i = 0; i <= beq - L; i++)
    std::swap(a[L + i], a[e - i]);
  for (int i = 0; i < R - eeq; i++)
    std::swap(a[R - i - 1], a[b + i]);
  return {L + e - beq - 1, b + R - eeq};
}

int kth(int L, int R, std::vector<int> &a, int k) {
  if (R - L == 1)
    return a[L];

  auto p = partition(L, R, a);
  if (k - 1 <= p.first - L)
    return kth(L, p.first + 1, a, k);
  else if (k - 1 < p.second - L)
    return a[p.first + 1];
  else
    return kth(p.second, R, a, k - (p.second - L));
}

int main() {
  int n, k;
  std::cin >> n >> k;

  int A, B, C, a1, a2;
  std::cin >> A >> B >> C >> a1 >> a2;

  std::vector<int> a(n);
  a[0] = a1, a[1] = a2;
  for (int i = 2; i < n; i++)
    a[i] = A * a[i - 2] + B * a[i - 1] + C;

  std::cout << kth(0, n, a, k) << std::endl;

  return 0;
}
