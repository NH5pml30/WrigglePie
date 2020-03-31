/* Kholiavin Nikolai, M3138 */
#include <array>
#include <fstream>
#include <functional>
#include <iostream>
#include <optional>
#include <random>
#include <set>
#include <stack>
#include <string>
#include <vector>

template <typename type>
class hash_provider;

template <typename type>
class hash_set {
 private:
  static constexpr size_t start_capacity = 10;
  std::vector<std::set<type>> data =
      std::vector<std::set<type>>(start_capacity);
  size_t size = 0;
  hash_provider<type> provider;
  std::function<size_t(const type &obj)> hash_function =
      provider.create_hash_function(start_capacity);

  void ensure_capacity(size_t new_size) {
    if (new_size >= data.size() * 3 / 4) {
      hash_function = provider.create_hash_function(data.size() * 4);
      auto new_data = std::vector<std::set<type>>(data.size() * 4);
      for (auto &list : data) {
        for (auto &el : list)
          new_data[hash_function(el)].emplace_back(std::move(el));
      }
      data.swap(new_data);
    }
  }

 public:
  hash_set() {}

  hash_set &operator=(const hash_set &) = delete;
  hash_set(const hash_set &) = delete;

  void insert(const type &x) {
    size_t h = hash_function(x);
    size += data[h].insert(x).second;
  }

  void insert(type &&x) {
    size_t h = hash_function(x);
    size += data[h].insert(std::move(x)).second;
  }

  void erase(const type &x) {
    size_t h = hash_function(x);
    size -= data[h].erase(x);
  }

  auto extract(const type &x) {
    size_t h = hash_function(x);
    auto res = data[h].extract(x);
    size -= (bool)res;
    return res;
  }

  bool exists(const type &x) const {
    size_t h = hash_function(x);
    return data[h].count(x) > 0;
  }

  const type &get(const type &x) const {
    size_t h = hash_function(x);
    return *data[h].find(x);
  }
};

size_t next_pow2(size_t x) {
  int i;
  for (i = sizeof(size_t) * 8 - 1; i >= 0; i--)
    if ((x >> i) & 1) break;
  return (size_t)1 << (i + 1);
}

template <typename key_type, typename val_type>
class comparable_entry {
 protected:
  key_type key;

 public:
  val_type val;

  comparable_entry(const key_type &key, const val_type &val)
      : key(key), val(val) {}
  comparable_entry(key_type &&key, val_type &&val)
      : key(std::move(key)), val(std::move(val)) {}
  comparable_entry(comparable_entry &&other) noexcept
      : key(std::move(other.key)), val(std::move(other.val)) {}
  comparable_entry(const comparable_entry &other)
      : key(other.key), val(other.val) {
    throw "darou";
  }
  comparable_entry &operator=(comparable_entry &&other) {
    if (this != &other) {
      key = std::move(other.key);
      val = std::move(other.val);
    }
    return *this;
  }

  virtual const key_type &getKey() const { return key; }

  bool operator<(const comparable_entry &other) const {
    return getKey() < other.getKey();
  }
};

template <typename key_type, typename val_type>
class comparable_entry<const key_type &, val_type>
    : public comparable_entry<key_type, val_type> {
 private:
  const key_type &key_ref;

 public:
  comparable_entry(const key_type &key_ref)
      : comparable_entry<key_type, val_type>(key_type(), val_type()),
        key_ref(key_ref) {}
  comparable_entry(comparable_entry &&other) noexcept
      : comparable_entry<key_type, val_type>(std::move(other)),
        key_ref(other.key_ref) {}
  comparable_entry(const comparable_entry &other)
      : comparable_entry<key_type, val_type>(other), key_ref(other.key_ref) {
    throw "darou";
  }
  comparable_entry &operator=(comparable_entry &&other) {
    if (this != &other) {
      key_ref = other.key_ref;
    }
    return *this;
  }

  const key_type &getKey() const override { return key_ref; }
};

template <typename key_type, typename val_type>
class hash_provider<comparable_entry<key_type, val_type>> {
  using obj_type = comparable_entry<key_type, val_type>;

 public:
  static std::function<size_t(const obj_type &obj)> create_hash_function(
      size_t M) {
    auto func = hash_provider<key_type>::create_hash_function(M);
    return [func](const obj_type &obj) { return func(obj.getKey()); };
  }
};

template <typename key_type, typename val_type>
class hash_map {
  static_assert(std::is_default_constructible_v<val_type>);
  using second_t = val_type;
  using entry_t = comparable_entry<key_type, val_type>;
  using search_entry_t = comparable_entry<const key_type &, val_type>;

 private:
  hash_set<entry_t> set;

 public:
  bool exists(const key_type &key) const {
    return set.exists(search_entry_t(key));
  }

  void put(const key_type &key, const val_type &val) {
    set.erase(search_entry_t(key));
    set.insert(entry_t(key, val));
  }
  void put(key_type &&key, val_type &&val) {
    set.erase(search_entry_t(key));
    set.insert(entry_t(std::move(key), std::move(val)));
  }

  const val_type &get(const key_type &key) const {
    return set.get(search_entry_t(key)).val;
  }

  void erase(const key_type &key, const val_type &val) {
    set.erase(search_entry_t(key));
  }
};

template <size_t primes_size>
constexpr std::array<size_t, primes_size> count_primes(int begin) {
  constexpr int MAX_NUMBER = 1000;
  bool not_primes[MAX_NUMBER + 1]{};
  for (int i = 2; i <= MAX_NUMBER; i++) {
    if (!not_primes[i]) {
      for (int j = 2 * i; j <= MAX_NUMBER; j += i) not_primes[j] = true;
    }
  }
  std::array<size_t, primes_size> res{};
  int j = 0;
  for (int i = begin; j < (int)primes_size && i <= MAX_NUMBER; i++)
    if (!not_primes[i]) res[j++] = i;
  return res;
}

template <>
class hash_provider<std::vector<size_t>> {
  using obj_type = std::vector<size_t>;

 public:
  static std::default_random_engine generator;
  static std::uniform_int_distribution<size_t> distr;

  static constexpr size_t primes_size = 30;
  static constexpr std::array<size_t, primes_size> primes =
      count_primes<primes_size>(19);

  static std::function<size_t(const obj_type &obj)> create_hash_function(
      size_t M) {
    size_t p = primes[distr(generator)];
    return [=](const obj_type &obj) -> size_t {
      size_t mult = 1, res = 0;
      for (auto it = obj.rbegin(); it != obj.rend(); it++, mult *= p)
        res += mult * *it;
      return res % M;
    };
  }
};

std::default_random_engine hash_provider<std::vector<size_t>>::generator;
std::uniform_int_distribution<size_t> hash_provider<std::vector<size_t>>::distr(
    0, primes_size - 1);

template <typename stacked_t, typename node_t>
void dfs(stacked_t from,
         std::function<stacked_t(node_t *adjoint, stacked_t prev)> action) {
  std::stack<std::pair<stacked_t, std::optional<stacked_t>>> stack;
  stack.push({from, {}});
  while (!stack.empty()) {
    auto[node, prev] = stack.top();
    stack.pop();
    for (auto adjoint : node)
      if (!prev.has_value() || adjoint != (node_t *)*prev)
        stack.push({action(adjoint, node), node});
  }
}

template <typename node_t>
void dfs(node_t *from,
         std::function<node_t *(node_t *adjoint, node_t *prev)> action) {
  struct for_dfs {
    node_t *node;

    for_dfs(node_t *node) : node(node) {}

    operator node_t *() { return node; }

    auto begin() { return node->begin(); }

    auto end() { return node->end(); }
  };

  dfs<for_dfs, node_t>(for_dfs(from),
                       [&action](node_t *adjoint, for_dfs prev) -> for_dfs {
                         return for_dfs(action(adjoint, prev.node));
                       });
}

struct NODE {
  NODE *parent;
  std::vector<NODE *> children;

  auto begin() { return children.begin(); }

  auto end() { return children.end(); }

  NODE(NODE *parent, const std::vector<NODE *> children)
      : parent(parent), children(children) {}

  std::vector<size_t> sorted_class_ids;
  size_t class_id = std::numeric_limits<size_t>::max();

  void init(std::vector<std::vector<NODE *>> &class2nodes) {
    if (children.size() == 0) {
      class_id = 0;
      class2nodes[0].push_back(this);
    }
  }

  static void init(NODE *root, std::vector<std::vector<NODE *>> &class2nodes) {
    dfs<NODE>(root, [&class2nodes](NODE *adjoint, NODE *) -> NODE * {
      adjoint->init(class2nodes);
      return adjoint;
    });
  }

  void update(hash_map<std::vector<size_t>, size_t> &class_map,
              std::vector<std::vector<NODE *>> &class2nodes) {
    if (parent != nullptr) {
      parent->sorted_class_ids.push_back(class_map.get(sorted_class_ids));
      if (parent->sorted_class_ids.size() == parent->children.size()) {
        if (class_map.exists(parent->sorted_class_ids)) {
          class2nodes[class_map.get(parent->sorted_class_ids)].push_back(
              parent);
        } else {
          class_map.put(parent->sorted_class_ids, class2nodes.size());
          class2nodes.push_back({parent});
        }
      }
    }
  }

  static void update(NODE *root,
                     hash_map<std::vector<size_t>, size_t> &class_map,
                     std::vector<std::vector<NODE *>> &class2nodes) {
    dfs<NODE>(root,
              [&class_map, &class2nodes](NODE *adjoint, NODE *) -> NODE * {
                adjoint->update(class_map, class2nodes);
                return adjoint;
              });
  }
};

NODE *build_tree(const std::vector<std::pair<size_t, size_t>> &edges) {
  struct NO_ROOT_NODE {
    std::vector<NO_ROOT_NODE *> adjoint;
    size_t depth = std::numeric_limits<size_t>::max();
    NO_ROOT_NODE *prev = nullptr;

    NO_ROOT_NODE(const std::vector<NO_ROOT_NODE *> &adjoint = {})
        : adjoint(adjoint) {}

    auto begin() { return adjoint.begin(); }

    auto end() { return adjoint.end(); }

    static NO_ROOT_NODE *find_farthest(NO_ROOT_NODE *from) {
      from->depth = 0;
      NO_ROOT_NODE *res = from;
      dfs<NO_ROOT_NODE>(
          from,
          [&res](NO_ROOT_NODE *adjoint, NO_ROOT_NODE *prev) -> NO_ROOT_NODE * {
            adjoint->depth = prev->depth + 1;
            adjoint->prev = prev;
            if (adjoint->depth > res->depth) res = adjoint;
            return adjoint;
          });

      return res;
    }

    static NODE *hang(NO_ROOT_NODE *from) {
      struct for_dfs {
        NO_ROOT_NODE *src;
        NODE *dst;

        for_dfs(NO_ROOT_NODE *src, NODE *dst) : src(src), dst(dst) {}

        operator NO_ROOT_NODE *() { return src; }

        auto begin() { return src->begin(); }

        auto end() { return src->end(); }
      };

      NODE *res = new NODE(nullptr, {});
      dfs<for_dfs, NO_ROOT_NODE>(
          for_dfs(from, res), [](NO_ROOT_NODE *node, for_dfs prev) -> for_dfs {
            auto newnode = new NODE(prev.dst, {});
            prev.dst->children.push_back(newnode);
            return for_dfs(node, newnode);
          });
      return res;
    }
  };

  std::vector<NO_ROOT_NODE> nodes(edges.size() + 1);
  for (auto &edge : edges) {
    nodes[edge.first].adjoint.push_back(&nodes[edge.second]);
    nodes[edge.second].adjoint.push_back(&nodes[edge.first]);
  }

  NO_ROOT_NODE
  *longest_path_begin = NO_ROOT_NODE::find_farthest(&nodes[0]),
  *longest_path_end = NO_ROOT_NODE::find_farthest(longest_path_begin);

  size_t d = longest_path_end->depth;
  if (d % 2 != 0) return nullptr;

  auto *middle = longest_path_end;
  for (size_t i = 0; i < d / 2; i++) middle = middle->prev;

  return NO_ROOT_NODE::hang(middle);
}

int main() {
  size_t n;
  std::cin >> n;

  if (n == 1) {
    std::cout << "NO" << std::endl;
    return 0;
  }

  std::vector<std::pair<size_t, size_t>> edges(n - 1);
  for (size_t i = 0; i < n - 1; i++) {
    std::cin >> edges[i].first >> edges[i].second;
    edges[i].first--;
    edges[i].second--;
  }

  NODE *tree = build_tree(edges);
  if (tree == nullptr) {
    std::cout << "NO" << std::endl;
    return 0;
  }
  std::vector<std::vector<NODE *>> class2nodes({{}});
  NODE::init(tree, class2nodes);
  hash_map<std::vector<size_t>, size_t> class_map;
  class_map.put({}, 0);
  for (size_t cl = 0; cl < class2nodes.size(); cl++)
    for (auto node : class2nodes[cl]) node->update(class_map, class2nodes);

  const auto &ids = tree->sorted_class_ids;
  size_t last_index = 0, last = ids[0];
  for (size_t i = 0; i < ids.size(); i++)
    if (ids[i] != last) {
      if ((i - last_index) % 2 != 0) {
        std::cout << "NO" << std::endl;
        return 0;
      }
      last_index = i;
      last = ids[i];
    }
  if ((ids.size() - last_index) % 2 != 0) {
    std::cout << "NO" << std::endl;
    return 0;
  }
  std::cout << "YES" << std::endl;
  return 0;
}
