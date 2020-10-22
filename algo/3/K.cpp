/* Nikolai Kholiavin, M3138 */
#include <iostream>
#include <vector>
#include <functional>

class binomial_heap {
 public:
  class node {
    friend class binomial_heap;
   private:
    struct DATA {
      int key;
      size_t id;

      DATA(int key, size_t id) : key(key), id(id) {
      }

      bool operator<(const DATA &other) const {
        return key < other.key || (key == other.key && id < other.id);
      }
    };

    DATA data;
    node *parent;
    std::vector<node *> children;
    binomial_heap *heap;

    static void decreaseKey(node *el, int newKey, std::vector<node *> &idMap) {
      el->data.key = newKey;
      while (el->parent != nullptr && el->data < el->parent->data) {
        std::swap(idMap[el->data.id], idMap[el->parent->data.id]);
        std::swap(el->data, el->parent->data);
        el = el->parent;
      }
    }

    static void increaseKey(node *el, int newKey, std::vector<node *> &idMap) {
      el->data.key = newKey;

      while (el->children.size() != 0) {
        node *min_child = nullptr;
        DATA min(std::numeric_limits<int>::max(), -1);
        for (size_t i = 0; i < el->children.size(); i++)
          if (el->children[i]->data < min) {
            min_child = el->children[i];
            min = el->children[i]->data;
          }

        if (min < el->data) {
          std::swap(idMap[el->data.id], idMap[min_child->data.id]);
          std::swap(el->data, min_child->data);
          el = min_child;
        } else {
          break;
        }
      }
    }

    node(int key, size_t id, node *parent, binomial_heap *heap) :
      data(key, id), parent(parent), heap(heap) {
    }

    size_t degree() {
      return children.size();
    }

    static node * merge(node *left, node *right, binomial_heap *heap) {
      if (left == nullptr) {
        if (right != nullptr)
          right->setHeap(heap);
        return right;
      }
      if (right == nullptr) {
        if (left != nullptr)
          left->setHeap(heap);
        return left;
      }
      if (right->data < left->data)
        std::swap(left, right);
      left->children.push_back(right);
      right->parent = left;
      left->setHeap(heap);
      return left;
    }

    void changeKey(int newKey, std::vector<node *> &idMap) {
      if (newKey > data.key)
        increaseKey(this, newKey, idMap);
      else
        decreaseKey(this, newKey, idMap);
    }

    DATA get() {
      return data;
    }

    binomial_heap * getHeap() {
      if (parent == nullptr)
        return heap;
      return parent->getHeap();
    }

    void setHeap(binomial_heap *newHeap) {
      if (parent == nullptr)
        heap = newHeap;
      else
        parent->setHeap(newHeap);
    }

    bool operator<(const node &right) const {
      return data < right.data;
    }

    void loseCustody(std::vector<node *> &orphans) {
      orphans.swap(children);
      for (auto child : orphans)
        child->parent = nullptr;
    }

    ~node() {
      for (auto child : children)
        delete child;
    }
  };

 private:
  std::vector<node *> trees;
  std::vector<node *> &idMap;

  binomial_heap(std::vector<node *> &&trees, std::vector<node *> &idMap) :
    trees(trees), idMap(idMap) {
  }

 public:
  binomial_heap(std::vector<node *> &idMap) : trees({nullptr}), idMap(idMap) {
  }

  void insert(int key, size_t id) {
    node *el = new node(key, id, nullptr, this);

    if (id >= idMap.size())
      idMap.resize(id + 1, nullptr);
    idMap[id] = el;
    binomial_heap temp(std::move(std::vector<node *>({el})),
                       idMap);
    merge(temp);
  }

  void merge(binomial_heap &right) {
    if (right.trees.size() > trees.size())
      trees.resize(right.trees.size(), nullptr);

    node *carry = nullptr;
    size_t deg;
    for (deg = 0; deg < right.trees.size(); deg++) {
      node *sum = node::merge(trees[deg], right.trees[deg], this);
      if (sum != nullptr && sum->degree() > deg) {
        trees[deg] = carry;
        carry = sum;
      } else {
        carry = node::merge(sum, carry, this);
        if (carry == nullptr || carry->degree() == deg) {
          trees[deg] = carry;
          carry = nullptr;
        } else {
          trees[deg] = nullptr;
        }
      }
    }
    for (; carry != nullptr; deg++) {
      if (deg >= trees.size())
        trees.resize(deg + 1, nullptr);
      carry = node::merge(trees[deg], carry, this);
      if (carry == nullptr || carry->degree() == deg) {
        trees[deg] = carry;
        carry = nullptr;
      } else {
        trees[deg] = nullptr;
      }
    }

    right.trees = std::vector<node *>({nullptr});
  }

  void changeKey(size_t id, int newKey) {
    idMap[id]->changeKey(newKey, idMap);
  }

  int getMin() {
    node::DATA min(std::numeric_limits<int>::max(), -1);
    for (auto tree : trees)
      if (tree != nullptr && tree->get() < min)
        min = tree->get();
    return min.key;
  }

  int extractMin() {
    size_t min_i = -1;
    node::DATA min(std::numeric_limits<int>::max(), -1);
    for (size_t i = 0; i < trees.size(); i++)
      if (trees[i] != nullptr && trees[i]->get() < min) {
        min_i = i;
        min = trees[i]->get();
      }

    std::vector<node *> collectedOrphans;
    trees[min_i]->loseCustody(collectedOrphans);
    delete trees[min_i];
    trees[min_i] = nullptr;
    while (trees.size() > 1 && trees.back() == nullptr)
      trees.pop_back();
    idMap[min.id] = nullptr;

    binomial_heap temp(std::move(collectedOrphans), idMap);
    merge(temp);

    return min.key;
  }

  void erase(size_t id) {
    changeKey(id, std::numeric_limits<int>::min());
    extractMin();
  }

  static binomial_heap * getHeap(std::vector<node *> idMap, size_t id) {
    return idMap[id]->getHeap();
  }

  ~binomial_heap() {
    for (auto tree : trees)
      if (tree != nullptr)
        delete tree;
  }
};

int main() {
  int n, m;
  std::cin >> n >> m;
  std::vector<binomial_heap *> heaps(n);
  std::vector<binomial_heap::node *> idMap;

  for (int i = 0; i < n; i++)
    heaps[i] = new binomial_heap(idMap);

  int id = 0;
  for (int k = 0; k < m; k++) {
    int command, a, b, v, i;
    std::cin >> command;
    switch (command) {
    case 0:
      std::cin >> a >> v;
      a--;
      heaps[a]->insert(v, id++);
      break;
    case 1:
      std::cin >> a >> b;
      a--, b--;
      heaps[b]->merge(*heaps[a]);
      break;
    case 2:
      std::cin >> i;
      i--;
      binomial_heap::getHeap(idMap, i)->erase(i);
      break;
    case 3:
      std::cin >> i >> v;
      i--;
      binomial_heap::getHeap(idMap, i)->changeKey(i, v);
      break;
    case 4:
      std::cin >> a;
      a--;
      std::cout << heaps[a]->getMin() << std::endl;
      break;
    case 5:
      std::cin >> a;
      a--;
      heaps[a]->extractMin();
    }
  }

  for (int i = 0; i < n; i++)
    delete heaps[i];

  return 0;
}
