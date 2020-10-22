#include <iostream>
#include <vector>
#include <algorithm>

int main() {
  int n;
  std::cin >> n;

  std::vector<int> a(n);

  for (int i = 0; i < n; i++)
    a[i] = i + 1;

  for (int i = 2; i < n; i++)
    std::swap(a[i / 2], a[i]);

  for (int i = 0; i < n; i++)
    std::cout << a[i] << " ";
  std::cout << std::endl;

  return 0;
}
