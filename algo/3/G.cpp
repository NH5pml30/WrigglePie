/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <algorithm>

class id_supplier {
 private:
  struct NODE {
    NODE *next;

    NODE() {
    }

    NODE(NODE *next) : next(next) {
    }
  };

  std::vector<NODE> ids;
  NODE *firstFree;

 public:
  id_supplier() : firstFree(nullptr) {
    ids.reserve(100);
  }

  size_t get() {
    if (firstFree == nullptr) {
      ids.push_back(NODE(nullptr));
      firstFree = &ids.back();
    }
    size_t res = firstFree - ids.data();
    firstFree = firstFree->next;
    return res;
  }

  void free(size_t id) {
    ids[id].next = firstFree;
    firstFree = &ids[id];
  }
};

template<class type>
  class binary_heap {
   private:
    struct DATA {
      type data;
      size_t id;

      DATA(const type &data, size_t id) : data(data), id(id) {
      }

      DATA() {
      }

      bool operator<(const DATA &Other) const {
        return data < Other.data;
      }
    };

    std::vector<DATA> data;
    std::vector<size_t> idMap;
    id_supplier idSupplier;

    void siftDown(size_t i) {
      for (size_t left = 2 * i + 1, right = 2 * i + 2;
           (left < data.size() && data[i] < data[left]) ||
           (right < data.size() && data[i] < data[right]);
           left = 2 * i + 1, right = 2 * i + 2) {
        size_t new_place;
        if (left >= data.size())
          new_place = right;
        else if (right >= data.size())
          new_place = left;
        else
          new_place = (data[left] < data[right] ? right : left);

        std::swap(idMap[data[i].id], idMap[data[new_place].id]);
        std::swap(data[i], data[new_place]);
        i = new_place;
      }
    }

    void siftUp(size_t i) {
      for (size_t parent = (i - 1) / 2;
           i > 0 && data[parent] < data[i];
           parent = (i - 1) / 2) {
        std::swap(idMap[data[i].id], idMap[data[parent].id]);
        std::swap(data[i], data[parent]);
        i = parent;
      }
    }

   public:
    binary_heap() {}

    size_t insert(const type &d) {
      size_t id = idSupplier.get();
      data.push_back({d, id});
      if (id == idMap.size())
        idMap.push_back(id);
      else
        idMap[id] = data.size() - 1;
      siftUp(data.size() - 1);
      return id;
    }

    DATA extractMax() {
      type res = data.front();
      idSupplier.free(res.id);
      idMap[res.id] = -1;
      data.front() = data.back();
      idMap[data.front().id] = 0;
      data.pop_back();

      if (data.size() > 0)
        siftDown(0);
      return res;
    }

    const type & getMax() {
      return data.front().data;
    }

    size_t size() { return data.size(); }

    void changeKey(size_t id, const type &newData) {
      if (idMap[id] == (size_t)-1)
        return;
      DATA &d = data[idMap[id]];
      bool
        less = newData < d.data,
        more = d.data < newData;
      d.data = newData;
      if (less)
        siftDown(idMap[id]);
      else if (more)
        siftUp(idMap[id]);
    }

    void erase(size_t id) {
      auto d = data.back();
      idMap[d.id] = idMap[id];
      data[idMap[id]].id = d.id;
      idSupplier.free(id);
      idMap[id] = -1;
      data.pop_back();
      changeKey(d.id, d.data);
    }
  };

int main() {
  int n, m;
  std::cin >> n >> m;

  struct RMQ {
    int i, j, q;

    RMQ(int i, int j, int q) : i(i), j(j), q(q) {
    }

    RMQ() {
    }

    bool operator<(const RMQ &Other) const {
      return q < Other.q;
    }
  };

  struct RMQ_wrapper {
    RMQ *data = nullptr;

    RMQ_wrapper(RMQ *data) : data(data) {
    }

    RMQ_wrapper() {
    }

    bool operator<(const RMQ_wrapper &Other) const {
      return *data < *Other.data;
    }
  };

  std::vector<std::pair<RMQ, size_t>> data(m);
  std::vector<std::vector<std::pair<RMQ, size_t> *>> begins(n), ends(n);
  binary_heap<RMQ_wrapper> heap;

  for (int k = 0; k < m; k++) {
    int i, j, q;
    std::cin >> i >> j >> q;
    i--, j--;
    data[k] = {RMQ(i, j, q), -1};
    begins[i].push_back(&data[k]);
    ends[j].push_back(&data[k]);
  }

  for (int i = 0; i < n; i++) {
    for (auto rmq : begins[i])
      rmq->second = heap.insert(&rmq->first);

    if (heap.size() == 0)
      std::cout << 0 << ' ';
    else
      std::cout << heap.getMax().data->q << ' ';

    for (auto rmq : ends[i])
      heap.erase(rmq->second);
  }

  return 0;
}
