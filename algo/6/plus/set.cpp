/* Kholiavin Nikolai, M3138 */
#include <array>
#include <functional>
#include <iostream>
#include <set>
#include <random>
#include <vector>
#include <fstream>
#include <string>

template <typename type>
class hash_provider {
 public:
  virtual std::function<size_t(const type& obj)> create_hash_function(
      size_t M) = 0;
};

template <typename type, class hash_provider_type>
class hash_set {
  static_assert(
      std::is_base_of<hash_provider<type>, hash_provider_type>::value);

 private:
  static constexpr size_t start_capacity = 10;
  std::vector<std::set<type>> data =
      std::vector<std::set<type>>(start_capacity);
  size_t size = 0;
  hash_provider_type provider = hash_provider_type();
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

  bool exists(const type& x) {
    size_t h = hash_function(x);
    return data[h].count(x) > 0;
  }
};

size_t next_pow2(size_t x) {
  int i;
  for (i = sizeof(size_t) * 8 - 1; i >= 0; i--)
    if ((x >> i) & 1) break;
  return (size_t)1 << (i + 1);
}

class provider : public hash_provider<int> {
 public:
  static std::default_random_engine generator;

  std::function<size_t(const int& obj)> create_hash_function(
      size_t M) override {
    size_t p = next_pow2(M) - 1;
    std::uniform_int_distribution<size_t> a_distr(1, p - 1), b_distr(0, p - 1);
    size_t a = a_distr(generator), b = b_distr(generator);
    return [=](const int& obj) -> size_t { return (a * obj + b) % p % M; };
  }
};

std::default_random_engine provider::generator;

int main() {
  static std::string task_name = "set";
  auto in = std::ifstream(task_name + ".in");
  auto out = std::ofstream(task_name + ".out");

  hash_set<int, provider> set;

  while (true) {
    std::string command;
    int num;
    if (!(in >> command >> num)) break;
    if (command == "insert")
      set.insert(num);
    else if (command == "delete")
      set.erase(num);
    else
      out << (set.exists(num) ? "true" : "false") << std::endl;
  }

  return 0;
}
