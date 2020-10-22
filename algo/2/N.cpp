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

  // indeterminately argument evaluation doesn't matter
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
  // indeterminately argument evaluation fix
  int wrote = BuildSort(Net, L, L + size / 2, Layer);
  Layer += std::max(wrote, BuildSort(Net, L + size / 2, L + size, Layer));

  if (Layer >= (int)Net.size())
    Net.push_back({});
  for (int i = 0; i < size / 2; i++)
    Net[Layer].push_back({L + i, L + size - 1 - i});
  Layer++;

  // indeterminately argument evaluation fix
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

void Print(std::vector<std::vector<std::pair<int, int>>> &Net, int N) {
  int m = 0;
  for (auto &layer : Net)
    m += layer.size();
  std::cout << N << " " << m << " " << Net.size() << std::endl;
  for (auto &layer : Net) {
    std::cout << layer.size();
    for (auto comp : layer)
      std::cout << " " << comp.first + 1 << " " << comp.second + 1;
    std::cout << std::endl;
  }
}

int main() {
  int n;
  std::cin >> n;
  std::vector<std::vector<std::pair<int, int>>> net;
  BuildSort(net, 0, n);
  Print(net, n);
  return 0;
}
