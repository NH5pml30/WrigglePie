/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>

class binary_heap {
 public:
  struct DATA {
    int l, r;

    DATA(int l, int r) : l(l), r(r) {}

    DATA() {}

    bool operator<(const DATA &other) const {
      return r - l < other.r - other.l;
    }

    bool operator>(const DATA &other) const {
      return r - l > other.r - other.l;
    }
  };

 private:
  std::vector<DATA> data;

  void siftDown(int i) {
    for (int left = 2 * i + 1, right = 2 * i + 2;
         (left < (int)data.size() && data[i] > data[left]) ||
         (right < (int)data.size() && data[i] > data[right]);
         left = 2 * i + 1, right = 2 * i + 2) {
      int new_place;
      if (left >= (int)data.size())
        new_place = right;
      else if (right >= (int)data.size())
        new_place = left;
      else
        new_place = (data[left] < data[right] ? left : right);

      std::swap(data[i], data[new_place]);
      i = new_place;
    }
  }

  void siftUp(int i) {
    for (int parent = (i - 1) / 2; data[i] < data[parent];
         parent = (i - 1) / 2) {
      std::swap(data[i], data[parent]);
      i = parent;
    }
  }

 public:
  binary_heap() {}

  void insert(const DATA &d) {
    data.push_back(d);
    siftUp(data.size() - 1);
  }

  DATA extractMin() {
    DATA res = data.front();
    data.front() = data.back();
    data.pop_back();
    siftDown(0);
    return res;
  }

  int size() { return data.size(); }

  DATA *find(int x) {
    for (auto &el : data)
      if (el.l <= x && x <= el.r) return &el;
    return nullptr;
  }

  void changeKey(DATA *tempPtr, const DATA &newData) {
    bool less = newData<*tempPtr, more = newData> * tempPtr;
    *tempPtr = newData;
    if (less)
      siftUp(tempPtr - data.data());
    else if (more)
      siftDown(tempPtr - data.data());
  }

  void erase(DATA *tempPtr) {
    auto d = data.back();
    data.pop_back();
    if (data.size() > 0) {
      changeKey(tempPtr, d);
    }
  }
};

class disjoint_set {
 private:
  std::vector<int> parents, ranks;
  binary_heap ranges;

 public:
  disjoint_set(int n) : parents(n), ranks(n) {
    int i = 0;
    for (auto &el : parents)
      el = i++;
  }

  int find(int x) {
    if (parents[x] == x)
      return x;
    return parents[x] = find(parents[x]);
  }

  void join(int x, int y) {
    if (y - x == 1)
      return joinRange(x, y);
    if (x - y == 1)
      return joinRange(y, x);

    x = find(x), y = find(y);
    if (x == y)
      return;

    if (ranks[x] > ranks[y]) {
      parents[y] = x;
    } else {
      parents[x] = y;
      if (ranks[x] == ranks[y])
        ranks[y]++;
    }
  }

  void joinRange(int x, int y) {
    auto left = ranges.find(x), right = ranges.find(y);
    int begin = x, end = y;

    if (left == nullptr) {
      if (right == nullptr) {
        ranges.insert({x, y});
      } else {
        end = right->l;
        if (x < right->l)
          ranges.changeKey(right, {x, right->r});
      }
    } else {
      if (right == nullptr) {
        begin = left->r;
        if (y > left->r)
          ranges.changeKey(left, {left->l, y});
      } else {
        if (left == right) return;
        begin = left->r;
        end = right->l;
        int save_l = left->l, save_r = right->r;
        ranges.erase(left);
        ranges.changeKey(ranges.find(y), {save_l, save_r});
      }
    }

    if (ranges.size() > 100) ranges.extractMin();

    int max1 = 0, max1_i = -1, max2 = 0, max2_i = -1;
    for (int i = begin; i <= end; i++) {
      int set = find(i);
      if (ranks[set] >= max1) {
        max2 = max1;
        max2_i = max1_i;
        max1 = ranks[set];
        max1_i = set;
      } else if (ranks[set] >= max2) {
        max2 = ranks[set];
        max2_i = set;
      }
    }

    if (max1_i != -1 && max2_i != -1) {
      for (int i = begin; i <= end; i++)
        parents[parents[i]] = max1_i;
      if (max1 == max2)
        ranks[max1_i]++;
    }
  }
};

int main() {
  std::ios::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n, q;
  std::cin >> n >> q;
  disjoint_set set(n);
  bool is_first = true;

  for (int i = 0; i < q; i++) {
    int type, x, y;
    std::cin >> type >> x >> y;
    switch (type) {
      case 1:
        set.join(x - 1, y - 1);
        break;
      case 2:
        set.joinRange(x - 1, y - 1);
        break;
      case 3:
        if (!is_first)
          std::cout << '\n';
        else
          is_first = false;
        std::cout << (set.find(x - 1) == set.find(y - 1) ? "YES" : "NO");
        break;
    }
  }

  std::cout << std::endl;
  return 0;
}
