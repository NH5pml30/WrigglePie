/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>

class binary_heap {
 public:
  struct DATA {
    int key, id;

    DATA(int key, int id) : key(key), id(id) {
    }

    DATA() {
    }

    bool operator<(const DATA &other) const {
      return key < other.key;
    }

    bool operator>(const DATA &other) const {
      return key > other.key;
    }
  };

 private:
  std::vector<DATA> data;
  std::vector<int> idMap;

  void siftDown(int i) {
    for (int left = 2 * i + 1, right = 2 * i + 2;
         (left < (int)data.size() && data[i] > data[left]) ||
           (right < (int)data.size() && data[i] > data[right]);
         left = 2 * i + 1, right = 2 * i + 2) {
      int new_place = right;
      if (data[left] < data[right])
        new_place = left;

      std::swap(idMap[data[i].id], idMap[data[new_place].id]);
      std::swap(data[i], data[new_place]);
      i = new_place;
    }
  }

  void siftUp(int i) {
    for (int parent = (i - 1) / 2; data[i] < data[parent]; parent = (i - 1) / 2) {
      std::swap(idMap[data[i].id], idMap[data[parent].id]);
      std::swap(data[i], data[parent]);
      i = parent;
    }
  }

 public:
  binary_heap() {
  }

  void insert(int key, int id) {
    data.push_back(DATA(key, id));
    idMap.resize(id + 1);
    idMap.back() = data.size() - 1;
    siftUp(data.size() - 1);
  }

  DATA extractMin() {
    DATA res = data.front();
    idMap[res.id] = -1;
    data.front() = data.back();
    data.pop_back();

    if (data.size() > 0) {
      idMap[data.front().id] = 0;
      siftDown(0);
    }
    return res;
  }

  int size() {
    return data.size();
  }

  void decreaseKey(int id, int newKey) {
    if (idMap[id] == -1)
      return;
    data[idMap[id]].key = newKey;
    siftUp(idMap[id]);
  }
};

int main() {
  binary_heap priority_queue;
  int id = 0;

  while (!std::cin.eof()) {
    std::string command;
    std::cin >> command;
    if (command == "push") {
      int x;
      std::cin >> x;
      priority_queue.insert(x, id);
    } else if (command == "extract-min") {
      if (priority_queue.size() == 0) {
        std::cout << "*" << std::endl;
      } else {
        auto data = priority_queue.extractMin();
        std::cout << data.key << " " << data.id + 1 << std::endl;
      }
    } else if (command == "decrease-key") {
      int x, v;
      std::cin >> x >> v;
      priority_queue.decreaseKey(x - 1, v);
    }

    id++;
  }

  return 0;
}
