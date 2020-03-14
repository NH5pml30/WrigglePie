/* Kholiavin Nikolai, M3138 */
#include <array>
#include <fstream>
#include <functional>
#include <iostream>
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
    size += data[h].insert(x).second;
  }

  void erase(const type& x) {
    size_t h = hash_function(x);
    size -= data[h].erase(x);
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
struct comparable_pair {
  key_type key;
  val_type val;
  comparable_pair(const key_type& key, const val_type& val)
      : key(key), val(val) {}
  comparable_pair(key_type&& key, val_type&& val)
      : key(std::move(key)), val(std::move(val)) {}
  comparable_pair(comparable_pair&& other)
      : key(std::move(other.key)), val(std::move(other.val)) {}
  comparable_pair(const comparable_pair& other)
      : key(other.key), val(other.val) {}

  bool operator<(const comparable_pair& other) const { return key < other.key; }
};

template <typename key_type, typename val_type>
class hash_provider<comparable_pair<key_type, val_type>> {
 public:
  static std::function<size_t(const comparable_pair<key_type, val_type>& obj)>
  create_hash_function(size_t M) {
    auto func = hash_provider<key_type>::create_hash_function(M);
    return [func](const comparable_pair<key_type, val_type>& obj) {
      return func(obj.key);
    };
  }
};

template <typename key_type, typename val_type>
class hash_map {
  static_assert(std::is_default_constructible_v<val_type>);
  using pair = comparable_pair<key_type, val_type>;

 private:
  hash_set<pair> set;

 public:
  bool exists(const key_type& key) { return set.exists(pair(key, val_type())); }
  bool exists(key_type&& key) {
    return set.exists(pair(std::move(key), val_type()));
  }

  void put(const key_type& key, const val_type& val) {
    auto p = pair(key, val);
    set.erase(p);
    set.insert(std::move(p));
  }
  void put(key_type&& key, val_type&& val) {
    auto p = pair(std::move(key), std::move(val));
    set.erase(p);
    set.insert(std::move(p));
  }

  const val_type& get(const key_type& key) {
    return set.get(pair(key, val_type())).val;
  }
  const val_type& get(key_type&& key) {
    return set.get(pair(std::move(key), val_type())).val;
  }

  void erase(const key_type& key) { set.erase(pair(key, val_type())); }
  void erase(key_type&& key) { set.erase(pair(std::move(key), val_type())); }
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
  static std::string task_name = "map";
  auto in = std::ifstream(task_name + ".in");
  auto out = std::ofstream(task_name + ".out");

  hash_map<std::string, std::string> map;

  while (true) {
    std::string command, key, val;
    if (!(in >> command >> key)) break;
    if (command == "put") {
      in >> val;
      map.put(key, val);
    } else if (command == "delete") {
      map.erase(key);
    } else {
      out << (map.exists(key) ? map.get(key) : "none") << std::endl;
    }
  }

  return 0;
}
