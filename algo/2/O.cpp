/* Kholiavin Nikolai, M3138 */

#include <iostream>
#include <vector>
#include <algorithm>

int BuildBitonicSort(std::vector<std::vector<std::pair<int, int>>> &Net,
                     int L, int R, int Layer = -1) {
  if (R - L == 1)
    return 0;

  if (Layer == -1)
    Layer = (int)Net.size();

  if (Layer >= (int)Net.size())
    Net.push_back({});
  Net[Layer].reserve(Net[Layer].size() + (R - L) / 2);
  for (int i = 0; i < (R - L) / 2; i++)
    Net[Layer].push_back({L + i, (R + L) / 2 + i});

  // indeterminate argument evaluation doesn't matter
  return 1 + std::max(BuildBitonicSort(Net, L, (L + R) / 2, Layer + 1),
    BuildBitonicSort(Net, (L + R) / 2, R, Layer + 1));
}

size_t NextPower2(size_t X) {
  X--;
  for (unsigned i = 0; i < sizeof(size_t); i++)
    X |= X >> (1 << i);
  X++;
  return X + (X == 0);
}

// Expects all comparators in Layer to be less than L
int BuildSort(std::vector<std::vector<std::pair<int, int>>> &Net, int L, int R, int Layer = -1) {
  if (R - L == 1)
    return 0;
  if (Layer == -1)
    Layer = (int)Net.size();

  int saved_layer = Layer;

  int size = (int)NextPower2(R - L);
  // indeterminate argument evaluation fix
  int wrote = BuildSort(Net, L, L + size / 2, Layer);
  Layer += std::max(wrote, BuildSort(Net, L + size / 2, L + size, Layer));

  if (Layer >= (int)Net.size())
    Net.push_back({});
  for (int i = 0; i < size / 2; i++)
    Net[Layer].push_back({L + i, L + size - 1 - i});
  Layer++;

  // indeterminate argument evaluation fix
  wrote = BuildBitonicSort(Net, L, L + size / 2, Layer);
  Layer += std::max(wrote, BuildBitonicSort(Net, L + size / 2, L + size, Layer));

  for (int lay = saved_layer; lay < Layer; lay++) {
    int cur = 0;
    for (int i = 0; i < (int)Net[lay].size(); i++) {
      if (cur != i)
        Net[lay][cur] = Net[lay][i];
      if (Net[lay][i].first < R && Net[lay][i].second < R)
        cur++;
    }
    Net[lay].resize(cur);
  }

  return Layer - saved_layer;
}

void Print(std::vector<std::vector<std::pair<int, int>>> &Net) {
  int m = 0;
  for (auto &layer : Net)
    m += layer.size();
  std::cout << m << std::endl;
  for (auto &layer : Net)
    for (auto &comp : layer)
      std::cout << " " << comp.first + 1 << " " << comp.second + 1 << std::endl;
}

int main() {
  while (true) {
    int n;
    std::cin >> n;
    if (n == 0)
      break;

    std::vector<bool> a(n);
    int first_1 = -1, k = 0;
    for (int i = 0; i < n; i++) {
      bool x;
      std::cin >> x;
      if (x && first_1 == -1)
        first_1 = i;

      k += (a[i] = x);
    }

    bool is_sorted = true;
    for (int i = 0; i < n - 1; i++)
      if (a[i] && !a[i + 1]) {
          is_sorted = false;
          break;
      }
    if (is_sorted) {
      std::cout << "-1" << std::endl;
      continue;
    }

    // set comparators with other ones
    std::vector<std::vector<std::pair<int, int>>> net;
    for (int i = first_1 + 1; i < n; i++)
      if (a[i])
        net.push_back({{first_1, i}});

    // sort all except first one
    int wrote = BuildSort(net, 0, n - 1);
    for (int i = 0; i < wrote; i++)
      for (auto &comp : net[net.size() - i - 1]) {
        if (comp.first >= first_1)
          comp.first++;
        if (comp.second >= first_1)
          comp.second++;
      }

    // set comparators to sift from first_1 to k + 1 until end
    for (int i = first_1; i < n - k - 1; i++)
      net.push_back({{i, i + 1}});

    // set comparators to sift from first_1 to start
    for (int i = first_1; i > 0; i--)
      net.push_back({{i, i - 1}});

    Print(net);
  }
  return 0;
}
