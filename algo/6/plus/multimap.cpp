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

template <typename key_type, typename val_type>
class comparable_entry {
 protected:
  key_type key;

 public:
  val_type val;

  comparable_entry(const key_type& key, const val_type& val)
      : key(key), val(val) {
  }
  comparable_entry(key_type&& key, val_type&& val)
      : key(std::move(key)), val(std::move(val)) {
  }
  comparable_entry(comparable_entry&& other) noexcept
      : key(std::move(other.key)), val(std::move(other.val)) {
  }
  comparable_entry(const comparable_entry& other)
      : key(other.key), val(other.val) {
    throw "darou";
  }
  comparable_entry& operator=(comparable_entry&& other) {
    if (this != &other) {
      key = std::move(other.key);
      val = std::move(other.val);
    }
    return *this;
  }

  virtual const key_type& getKey() const {
    return key;
  }

  bool operator<(const comparable_entry& other) const {
    return getKey() < other.getKey();
  }
};

template <typename key_type, typename val_type>
class comparable_entry<const key_type &, val_type> : public comparable_entry<key_type, val_type> {
 private:
  const key_type& key_ref;

 public:
  comparable_entry(const key_type& key_ref)
      : comparable_entry<key_type, val_type>(key_type(), val_type()),
        key_ref(key_ref) {}
  comparable_entry(comparable_entry&& other) noexcept
      : comparable_entry<key_type, val_type>(std::move(other)),
        key_ref(other.key_ref) {}
  comparable_entry(const comparable_entry& other)
      : comparable_entry<key_type, val_type>(other), key_ref(other.key_ref) {
    throw "darou";
  }
  comparable_entry& operator=(comparable_entry&& other) {
    if (this != &other) {
      key_ref = other.key_ref;
    }
    return *this;
  }

  const key_type& getKey() const override {
    return key_ref;
  }
};

template <typename key_type, typename val_type>
class hash_provider<comparable_entry<key_type, std::set<val_type>>> {
  using obj_type = comparable_entry<key_type, std::set<val_type>>;

 public:
  static std::function<size_t(const obj_type& obj)> create_hash_function(
      size_t M) {
    auto func = hash_provider<key_type>::create_hash_function(M);
    return [func](const obj_type& obj) { return func(obj.getKey()); };
  }
};

template <typename key_type, typename val_type>
class hash_multimap {
  static_assert(std::is_default_constructible_v<val_type>);
  using second_t = std::set<val_type>;
  using entry_t = comparable_entry<key_type, std::set<val_type>>;
  using search_entry_t = comparable_entry<const key_type &, std::set<val_type>>;

 private:
  hash_set<entry_t> set;

 public:
  bool exists(const key_type& key) {
    return set.exists(search_entry_t(key));
  }

  void put(const key_type& key, const val_type& val) {
    auto node = set.extract(search_entry_t(key));
    if (!node) {
      set.insert(entry_t(key, {val}));
    } else {
      entry_t p = std::move(node.value());
      p.val.insert(val);
      set.insert(std::move(p));
    }
  }
  void put(key_type &&key, val_type&& val) {
    auto node = set.extract(search_entry_t(key));
    if (!node) {
      set.insert(entry_t(std::move(key), std::move(val)));
    } else {
      entry_t p = std::move(node.value());
      p.val.emplace(std::move(val));
      set.insert(std::move(p));
    }
  }

  const std::set<val_type>& get(const key_type& key) {
    return set.get(search_entry_t(key)).val;
  }

  void erase(const key_type& key, const val_type& val) {
    auto pp = search_entry_t(key);
    auto node = set.extract(pp);
    if (node) {
      entry_t p = std::move(node.value());
      p.val.erase(val);
      set.insert(std::move(p));
    }
  }

  void erase_all(const key_type& key) { set.erase(search_entry_t(key)); }
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
  static std::string task_name = "multimap";
  auto in = std::ifstream(task_name + ".in");
  auto out = std::ofstream(task_name + ".out");

  hash_multimap<std::string, std::string> map;

  while (true) {
    std::string command, key, val;
    if (!(in >> command >> key)) break;
    if (command == "put") {
      in >> val;
      map.put(key, val);
    } else if (command == "delete") {
      in >> val;
      map.erase(key, val);
    } else if (command == "get") {
      if (map.exists(key)) {
        auto& set = map.get(key);
        out << set.size() << ' ';
        for (auto& val : set) out << val << ' ';
        out << std::endl;
      } else {
        out << "0" << std::endl;
      }
    } else if (command == "deleteall") {
      map.erase_all(key);
    }
  }

  return 0;
}
