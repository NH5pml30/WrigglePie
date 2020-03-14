/* Kholiavin Nikolai, M3138 */
#include <array>
#include <fstream>
#include <functional>
#include <iostream>
#include <optional>
#include <random>
#include <set>
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
  std::function<size_t(const type& obj)> hash_function =
      provider.create_hash_function(start_capacity);

  void ensure_capacity(size_t new_size) {
    if (new_size >= data.size() * 3 / 4) {
      hash_function = provider.create_hash_function(data.size() * 4);
      auto new_data = std::vector<std::set<type>>(data.size() * 4);
      for (auto& list : data) {
        for (auto& el : list)
          new_data[hash_function(el)].emplace_back(std::move(el));
      }
      data.swap(new_data);
    }
  }

 public:
  hash_set() {}

  hash_set& operator=(const hash_set&) = delete;
  hash_set(const hash_set&) = delete;

  void insert(const type& x) {
    size_t h = hash_function(x);
    size += data[h].insert(x).second;
  }

  void insert(type&& x) {
    size_t h = hash_function(x);
    size += data[h].insert(std::move(x)).second;
  }

  void erase(const type& x) {
    size_t h = hash_function(x);
    size -= data[h].erase(x);
  }

  auto extract(const type& x) {
    size_t h = hash_function(x);
    auto res = data[h].extract(x);
    size -= (bool)res;
    return res;
  }

  bool exists(const type& x) {
    size_t h = hash_function(x);
    return data[h].count(x) > 0;
  }

  const type& get(const type& x) {
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

template <class node_type>
struct list;

template <class data_type>
class node {
 private:
  void move(node&& other) {
    enclosing = other.enclosing;
    prev = other.prev;
    next = other.next;

    other.next = other.prev = nullptr;
    other.enclosing = nullptr;
    if (enclosing == nullptr) return;
    if (next != nullptr) {
      next->prev = this;
    } else {
      enclosing->tail = this;
    }
    if (prev != nullptr) {
      prev->next = this;
    } else {
      enclosing->head = this;
    }
  }

 public:
  list<data_type>* enclosing;
  node *prev, *next;
  data_type data;

  void append(list<data_type>* enclosing = nullptr, node* prev = nullptr,
              node* next = nullptr) {
    this->enclosing = enclosing;
    this->prev = prev;
    this->next = next;

    if (enclosing == nullptr) return;
    if (prev == enclosing->tail) enclosing->tail = this;
    if (next == enclosing->head) enclosing->head = this;
    if (prev != nullptr) prev->next = this;
    if (next != nullptr) next->prev = this;
  }

  node(node&& other) noexcept : data(std::move(other.data)) { move(std::move(other)); }
  node& operator=(node&& other) {
    if (this != &other) {
      data = std::move(other.data);
      move();
    }
    return *this;
  }

  node(const node& other) { throw "darou"; }

  node(const data_type& data, list<data_type>* enclosing = nullptr,
       node* prev = nullptr, node* next = nullptr)
      : data(data) {
    append(enclosing, prev, next);
  }
  node(data_type&& data, list<data_type>* enclosing = nullptr,
       node* prev = nullptr, node* next = nullptr)
      : data(std::move(data)) {
    append(enclosing, prev, next);
  }

  ~node() {
    if (enclosing == nullptr) return;

    if (prev != nullptr)
      prev->next = next;
    else
      enclosing->head = next;
    if (next != nullptr)
      next->prev = prev;
    else
      enclosing->tail = prev;
  }

  bool operator<(const node<data_type>& other) const {
    return data < other.data;
  }
};

template <class data_type>
struct list {
  node<data_type>*head = nullptr, *tail = nullptr;
};

template <typename key_type, typename val_type>
struct comparable_pair {
  key_type key;
  val_type val;
  comparable_pair(const key_type& key, const val_type& val)
      : key(key), val(val) {}
  comparable_pair(key_type&& key, val_type&& val)
      : key(std::move(key)), val(std::move(val)) {}
  comparable_pair(comparable_pair&& other) noexcept
      : key(std::move(other.key)), val(std::move(other.val)) {}
  comparable_pair(const comparable_pair& other)
      : key(other.key), val(other.val) {
    throw "darou";
  }
  comparable_pair& operator=(comparable_pair&& other) {
    if (this != &other) {
      key = std::move(other.key);
      val = std::move(other.val);
    }
    return *this;
  }

  bool operator<(const comparable_pair& other) const { return key < other.key; }
};

template <typename key_type, typename val_type>
class hash_provider<node<comparable_pair<key_type, val_type>>> {
  using obj_type = node<comparable_pair<key_type, val_type>>;

 public:
  static std::function<size_t(const obj_type& obj)> create_hash_function(
      size_t M) {
    auto func = hash_provider<key_type>::create_hash_function(M);
    return [func](const obj_type& obj) { return func(obj.data.key); };
  }
};

template <typename key_type, typename val_type>
class linked_hash_map {
  static_assert(std::is_default_constructible_v<val_type>);
  using pair_t = comparable_pair<key_type, val_type>;
  using node_t = node<pair_t>;

 private:
  hash_set<node_t> set;
  list<pair_t> linked_list;

 public:
  bool exists(const key_type& key) {
    return set.exists(node_t(pair_t(key, val_type())));
  }
  bool exists(key_type&& key) {
    return set.exists(node_t(pair_t(std::move(key), val_type())));
  }

  void put(const key_type& key, const val_type& val) {
    auto pp = node_t(pair_t(key, val));
    auto node = set.extract(pp);
    if (!node) {
      pp.append(&linked_list, linked_list.tail);
      set.insert(std::move(pp));
    } else {
      node_t p = std::move(node.value());
      p.data = std::move(pp.data);
      set.insert(std::move(p));
    }
  }
  void put(key_type&& key, val_type&& val) {
    auto pp = node_t(pair_t(std::move(key), std::move(val)));
    auto node = set.extract(pp);
    if (!node) {
      pp.append(&linked_list, linked_list.tail);
      set.insert(std::move(pp));
    } else {
      node_t p = std::move(node.value());
      p.data = std::move(pp.data);
      set.insert(std::move(p));
    }
  }

  const val_type& get(const key_type& key) {
    return set.get(node_t(pair_t(key, val_type()))).data.val;
  }
  const val_type& get(key_type&& key) {
    return set.get(node_t(pair_t(std::move(key), val_type()))).data.val;
  }

  void erase(const key_type& key) { set.erase(node_t(pair_t(key, val_type()))); }

  void erase(key_type&& key) { set.erase(node_t(pair_t(std::move(key), val_type()))); }

  std::optional<val_type> next(const key_type& key) {
    auto pp = node_t(pair_t(key, val_type()));
    if (!set.exists(pp)) return {};
    auto& p = set.get(std::move(pp));
    if (p.next == nullptr) return {};
    return static_cast<node_t*>(p.next)->data.val;
  }
  std::optional<val_type> next(key_type&& key) {
    auto pp = node_t(pair_t(std::move(key), val_type()));
    if (!set.exists(pp)) return {};
    auto& p = set.get(std::move(pp));
    if (p.next == nullptr) return {};
    return static_cast<node_t*>(p.next)->data.val;
  }

  std::optional<val_type> prev(const key_type& key) {
    auto pp = node_t(pair_t(key, val_type()));
    if (!set.exists(pp)) return {};
    auto& p = set.get(std::move(pp));
    if (p.prev == nullptr) return {};
    return static_cast<node_t*>(p.prev)->data.val;
  }
  std::optional<val_type> prev(key_type&& key) {
    auto pp = node_t(pair_t(std::move(key), val_type()));
    if (!set.exists(pp)) return {};
    auto& p = set.get(std::move(pp));
    if (p.prev == nullptr) return {};
    return static_cast<node_t*>(p.prev)->data.val;
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
class hash_provider<std::string> {
 public:
  static std::default_random_engine generator;
  static std::uniform_int_distribution<size_t> distr;

  static constexpr size_t primes_size = 30;
  static constexpr std::array<size_t, primes_size> primes =
      count_primes<primes_size>(19);

  static std::function<size_t(const std::string& obj)> create_hash_function(
      size_t M) {
    size_t p = primes[distr(generator)];
    return [=](const std::string& obj) -> size_t {
      size_t mult = 1, res = 0;
      for (int i = (int)obj.length() - 1; i >= 0; i--, mult *= p)
        res += mult * obj[i];
      return res % M;
    };
  }
};

std::default_random_engine hash_provider<std::string>::generator;
std::uniform_int_distribution<size_t> hash_provider<std::string>::distr(
    0, primes_size - 1);

int main() {
  static std::string task_name = "linkedmap";
  auto in = std::ifstream(task_name + ".in");
  auto out = std::ofstream(task_name + ".out");

  linked_hash_map<std::string, std::string> map;

  while (true) {
    std::string command, key, val;
    if (!(in >> command >> key)) break;
    if (command == "put") {
      in >> val;
      map.put(key, val);
    } else if (command == "delete") {
      map.erase(key);
    } else if (command == "get") {
      out << (map.exists(key) ? map.get(key) : "none") << std::endl;
    } else if (command == "next") {
      auto v = map.next(key);
      out << (v.has_value() ? *v : "none") << std::endl;
    } else if (command == "prev") {
      auto v = map.prev(key);
      out << (v.has_value() ? *v : "none") << std::endl;
    }
  }

  return 0;
}
