/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <stack>
#include <list>
#include <cstring>
#include <chrono>

int main() {
  std::istream &in = std::cin;
  std::ostream &out = std::cout;

  int n, m;
  in >> n >> m;

  if (n > 501) {
    std::cout << 0 << std::endl;
    return 0;
  }

  struct NODE;
  struct ROAD_DATA {
    int Min, Max;
    NODE *Left, *Right;
    int Covered = 0;

    ROAD_DATA(int Min, int Max, NODE *Left, NODE *Right) :
      Min(Min), Max(Max), Left(Left), Right(Right) {
    }

    ROAD_DATA() {
    }
  };
  std::vector<ROAD_DATA> road_data;
  road_data.reserve(n - 1);

  struct NODE {
    std::vector<NODE *> Adjoint;

    int SubtreeSize = -1;

    NODE(std::vector<NODE *> Adjoint) :
      Adjoint(Adjoint) {
    }

    NODE() {
    }
  };

  std::vector<NODE> nodes(n);

  for (int i = 0; i < n - 1; i++) {
    int a, b, l, r;
    in >> a >> b >> l >> r;
    a--, b--;
    road_data.push_back({l, r, &nodes[b], &nodes[a]});
    nodes[b].Adjoint.push_back(&nodes[a]);
    nodes[a].Adjoint.push_back(&nodes[b]);
  }

  std::vector<std::pair<NODE *, NODE *>> stack;
  stack.reserve(n);
  stack.push_back({&nodes[0], nullptr});
  while (!stack.empty()) {
    auto[node, last] = stack.back();

    bool ready = true;
    int sum = 0;
    for (auto ch : node->Adjoint)
      if (ch != last) {
        if (ch->SubtreeSize < 0) {
          ready = false;
          stack.push_back({ch, node});
        }
        sum += ch->SubtreeSize;
      }
    if (ready) {
      node->SubtreeSize = sum + 1;
      stack.pop_back();
    }
  }

  long long minsum = 0;
  for (auto &road : road_data) {
    long long side = std::min(road.Left->SubtreeSize, road.Right->SubtreeSize);
    side *= (n - side) * 2;
    if (road.Max < road.Min || minsum > m || minsum > LLONG_MAX - side * road.Min) {
      minsum = m + 1;
      break;
    }
    minsum += side * road.Min;
    road.Covered = (int)side;
    road.Max -= road.Min, road.Min = 0;
  }

  if (minsum > m) {
    std::cout << 0 << std::endl;
    return 0;
  } else if (minsum == m) {
    std::cout << 1 << std::endl;
    return 0;
  }

  m -= (int)minsum;

  std::vector<int> v1(m + 1), v2(m + 1), *f = &v1, *g = &v2;
  constexpr int mod = 1'000'000'007;
  (*g)[0] = 1;

  for (int i = 0; i < n - 1; i++) {
    memcpy(f->data(), g->data(), std::min(m + 1, (road_data[i].Covered - 1)) * sizeof(int));

    for (int w = road_data[i].Covered; w <= m; w++) {
      (*f)[w] = ((*f)[w - road_data[i].Covered] + (*g)[w]) % mod;
      if (int ind = w / road_data[i].Covered - 1;
          ind >= road_data[i].Max)
        (*f)[w] = ((*f)[w] + mod - (*g)[w - road_data[i].Covered * (road_data[i].Max + 1)]) % mod;
    }
    std::swap(f, g);
  }

  out << (*g)[m] << std::endl;

  return 0;
}
