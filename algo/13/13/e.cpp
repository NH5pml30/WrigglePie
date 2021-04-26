/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <string>
#include <array>
#include <set>
#include <stack>
#include <optional>
#include <variant>

int32_t round_up_pow2(int32_t x) {
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x++;
  return x;
}

template<typename OutT>
std::vector<OutT> find_suffix_array(std::string str) {
  size_t saved_size = str.size();

  str.resize(round_up_pow2((int32_t)str.size() + 1), '$');
  size_t n = str.size();

  std::vector<OutT> c;
  std::transform(str.begin(), str.end(), std::back_inserter(c), [](char ch) { return (OutT)ch; });

  std::vector<OutT> h(std::max(n, (size_t)('z' + 1)), 0);
  for (auto clazz : c) h[clazz]++;
  std::transform(h.begin(), h.end(), h.begin(), [last = (OutT)0](OutT val) mutable {
    auto save = last;
    last = (OutT)(last + val);
    return save;
  });

  std::vector<OutT> p(str.size());
  {
    std::vector<OutT> h_copy = h;
    OutT i = 0;
    for (auto ch : str) p[h_copy[ch]++] = i++;
  }

  std::vector<OutT> p_prime(str.size());
  std::vector<std::pair<OutT, OutT>> t(str.size());
  for (size_t pow2 = 1; pow2 < n; pow2 *= 2) {
    t.clear();
    std::transform(c.begin(), c.end(), std::back_inserter(t), [&, i = 0](OutT clazz) mutable {
      return std::make_pair(clazz, c[(i++ + pow2) % n]);
    });

    {
      for (auto pi : p) {
        OutT sorted_lsd = (OutT)((pi - pow2 + n) % n);
        p_prime[h[c[sorted_lsd]]++] = sorted_lsd;
      }

      p.swap(p_prime);
    }

    std::pair<OutT, OutT> last = {(OutT)-1, (OutT)-1};
    auto clazz = (OutT)-1, i = (OutT)-1;

    for (auto pi : p) {
      ++i;
      if (t[pi] != last) {
        h[++clazz] = i;
        last = t[pi];
      }
      c[pi] = clazz;
    }
  }

  std::vector<OutT> res(saved_size);
  std::copy(p.begin() + n - saved_size, p.end(), res.begin());
  return res;
}

template<typename OutT>
std::vector<OutT> buildLCP(const std::string &str, const std::vector<OutT> &suf_arr) {
  std::vector<OutT> suf_inv(suf_arr.size());
  {
    OutT i = 0;
    for (auto si : suf_arr) suf_inv[si] = i++;
  }

  std::vector<OutT> lcp(str.size() - 1);
  OutT k = 0;
  for (int i = 0; i < (int)str.size(); i++) {
    OutT i_prime = suf_inv[i];
    if (i_prime == (int)(str.size() - 1)) {
      k = 0;
      continue;
    }

    OutT j = suf_arr[i_prime + 1];
    k = std::max<OutT>(0, (OutT)(k - 1));
    while (i + k < (int)str.size() && j + k < (int)str.size() && str[i + k] == str[j + k])
      k++;
    lcp[i_prime] = k;
  }

  return lcp;
}

template<typename type>
class monoid {
 private:
  std::function<type(const type &, const type &)> func;
  type identity_element;

 public:
  monoid() {}
  monoid(const std::function<type(const type &, const type &)> &func, type identity_element) :
    func(func), identity_element(std::move(identity_element)) {
  }

  type operator()(const type &lhs, const type &rhs) const {
    if (lhs == identity_element)
      return rhs;
    if (rhs == identity_element)
      return lhs;
    return func(lhs, rhs);
  }

  const type & identity() const {
    return identity_element;
  }
};

template<typename type, typename ranged_op>
class segment_tree {
 protected:
  std::vector<type> nodes;
  std::vector<ranged_op> pending_ops;
  int size;
  monoid<type> func;
  monoid<ranged_op> op_compose;
  std::function<type(const type &, const ranged_op &)> op_apply;

  static int to_power2(int x) {
    x--;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    return x + 1;
  }

  void propagate_(int x, int lx, int rx) {
    if (rx - lx == 1) return;
    if (pending_ops[x] == op_compose.identity()) return;
    apply_(2 * x + 1, pending_ops[x]);
    apply_(2 * x + 2, pending_ops[x]);
    pending_ops[x] = op_compose.identity();
  }

  void apply_(int x, const ranged_op &op) {
    nodes[x] = op_apply(nodes[x], op);
    pending_ops[x] = op_compose(pending_ops[x], op);
  }

  void apply_(int l, int r, const ranged_op &op, int x, int lx, int rx) {
    propagate_(x, lx, rx);
    if (l >= rx || lx >= r) return;
    if (lx >= l && rx <= r) {
      apply_(x, op);
      return;
    }

    int m = (lx + rx) / 2;
    apply_(l, r, op, 2 * x + 1, lx, m);
    apply_(l, r, op, 2 * x + 2, m, rx);
    nodes[x] = func(nodes[2 * x + 1], nodes[2 * x + 2]);
  }

  type eval_(int l, int r, int x, int lx, int rx) {
    propagate_(x, lx, rx);
    if (l >= rx || lx >= r)
      return func.identity();
    if (lx >= l && rx <= r)
      return nodes[x];
    int m = (lx + rx) / 2;
    return func(eval_(l, r, 2 * x + 1, lx, m), eval_(l, r, 2 * x + 2, m, rx));
  }

 public:
  segment_tree(const std::vector<type> &a, const monoid<type> &func,
               const monoid<ranged_op> &op_compose,
               const std::function<type(const type &, const ranged_op &)> &op_apply) :
    func(func), op_compose(op_compose), op_apply(op_apply) {
    int pow2 = to_power2((int)a.size());
    size = pow2;

    pending_ops.resize(2 * pow2 - 1, op_compose.identity());

    nodes.reserve(2 * pow2 - 1);
    nodes.resize(pow2 - 1);
    std::copy(a.begin(), a.end(), std::back_inserter(nodes));
    nodes.resize(2 * pow2 - 1, func.identity());
    int begin = pow2 - 1;
    pow2 /= 2;
    begin -= pow2;
    for (; pow2 != 0; begin -= (pow2 /= 2))
      for (int i = 0; i < pow2; i++)
        nodes[begin + i] = func(nodes[2 * (begin + i) + 1], nodes[2 * (begin + i) + 2]);
  }

  void apply(int l, int r, const ranged_op &op) {
    apply_(l, r, op, 0, 0, size);
  }

  type eval(int l, int r) {
    return eval_(l, r, 0, 0, size);
  }

  std::vector<type> export_data() {
    for (int i = 0; i < size - 1; i++)
      propagate_(i, 0, size);
    return std::vector<type>(nodes.begin() + (size - 1), nodes.end());
  }
};


template<typename type, class Comp>
class sparse_table {
 private:
  static int log2(int n) {
    int res = 0;
    while (n > 0) {
      n >>= 1;
      res++;
    }
    return res - 1;
  }

  std::vector<int> logs;
  Comp func;
  std::vector<std::vector<type>> f;

 public:
  sparse_table(const std::vector<type> &a, Comp func)
      : logs(a.size() + 1), func(std::move(func)), f(a.size()) {
    int n = (int)a.size();
    logs[1] = 0;
    for (int i = 2; i <= n; i++) logs[i] = logs[i / 2] + 1;

    for (int i = 0; i < n; i++) {
      f[i].resize(logs[n - i] + 1);
      f[i][0] = a[i];
    }

    for (int k = 1, pow_k_minus_1 = 1; k <= logs[n]; k++, pow_k_minus_1 *= 2)
      for (int l = 0; l < n && k < logs[n - l] + 1; l++)
        f[l][k] = this->func(f[l][k - 1], f[l + pow_k_minus_1][k - 1]);
  }

  type eval(int l, int r) {
    int k = logs[r - l];
    return func(f[l][k], f[r - (1 << k)][k]);
  }
};

template<typename type, class Comp>
sparse_table(const std::vector<type> &, Comp) -> sparse_table<type, Comp>;



int main() {
  int n, m;
  std::cin >> n >> m;

  std::string s(n, '\0');
  for (int i = 0; i < n; i++) {
    int val;
    std::cin >> val;
    s[i] = (char)('a' + val);
  }

  auto arr = find_suffix_array<int>(s);
  auto lcp = buildLCP(s, arr);

  struct val_with_max {
    int val = std::numeric_limits<int>::min(), max = val, stamp = 0, max_stamp = 0;

    val_with_max() = default;
    val_with_max(int val, int stamp = 0) : val(val), max(val), stamp(stamp), max_stamp(stamp) {}

    val_with_max &set(int new_val) {
      stamp++;
      if (new_val > max)
        max = new_val, max_stamp = stamp;
      val = new_val;
      return *this;
    }

    val_with_max &offset(int delta) {
      val += delta;
      max += delta;
      return *this;
    }

    val_with_max &apply_history(val_with_max history) {
      if (history.max > max)
        max = history.max, max_stamp = history.max_stamp;
      return *this;
    }

    bool operator==(val_with_max other) const {
      // called with neutral
      return val == other.val;
    }
  };

  struct add_or_set {
    std::variant<int, int> data;
    val_with_max historic_max_add{};
    val_with_max historic_max_set{};

    int &get_data() {
      return data.index() == 0 ? std::get<0>(data) : std::get<1>(data);
    }
    int get_data() const {
      return data.index() == 0 ? std::get<0>(data) : std::get<1>(data);
    }

    static val_with_max apply(val_with_max val, add_or_set op) {
      val.apply_history(op.historic_max_add.offset(val.val));
      val.apply_history(op.historic_max_set);

      if (op.data.index() == 0)
        val.set(val.val + op.get_data());
      else
        val.set(op.get_data());
      return val;
    }

    bool operator==(add_or_set other) const {
      return data == other.data;
    }
    bool operator!=(add_or_set other) const {
      return data != other.data;
    }

    static add_or_set neutral() {
      return {std::variant<int, int>(std::in_place_index<0>, 0)};
    }

    static add_or_set add(int val, int stamp = 0) {
      add_or_set res = {std::variant<int, int>(std::in_place_index<0>, val)};
      res.historic_max_add = val_with_max(val, stamp);
      return res;
    }

    static add_or_set set(int val, int stamp = 0) {
      add_or_set res = {std::variant<int, int>(std::in_place_index<1>, val)};
      res.historic_max_set = val_with_max(val, stamp);
      return res;
    }

    static add_or_set compose(add_or_set lhs, add_or_set rhs) {
      int flags = (lhs.data.index() << 1) | rhs.data.index();

      switch (flags) {
        case 0b00:
          lhs.historic_max_add.apply_history(rhs.historic_max_add.offset(lhs.get_data()));
          lhs.get_data() += rhs.get_data();
          break;
        case 0b01:
          lhs.historic_max_add.apply_history(rhs.historic_max_add.offset(lhs.get_data()));
          lhs.historic_max_set = rhs.historic_max_set;
          lhs.data = rhs.data;
          break;
        case 0b10:
          lhs.historic_max_set.apply_history(rhs.historic_max_add.offset(lhs.get_data()));
          lhs.get_data() += rhs.get_data();
          break;
        default:  // case 0b11:
          lhs.historic_max_set.apply_history(rhs.historic_max_add.offset(lhs.get_data()));
          lhs.historic_max_set.apply_history(rhs.historic_max_set);
          lhs.get_data() = rhs.get_data();
          break;
      }
      return lhs;
    }
  };

  segment_tree<val_with_max, add_or_set> range(
      std::vector<val_with_max>(n, val_with_max(0)),
      {[](auto x, auto y) { return std::max(x.max, y.max); }, {std::numeric_limits<int>::min()}},
      {add_or_set::compose, add_or_set::neutral()}, add_or_set::apply);

  lcp.push_back(-1);
  for (int i = 0; i < n; i++) {
    range.apply(lcp[i] + 1, n, add_or_set::set(0, i + 1));
    range.apply(0, lcp[i] + 1, add_or_set::add(1, i + 1));
  }

  std::vector<val_with_max> res = range.export_data();
  int64_t ans = 0;
  int ans_l = -1, ans_stamp = -1;
  for (int l = 0; l < n; l++)
    if ((int64_t)l * (res[l].max + 1) >= ans)
      ans = (int64_t)l * (res[l].max + 1), ans_l = l, ans_stamp = res[l].max_stamp;

  int offset;
  if (ans < n)
    ans = n, ans_l = n, offset = 0;
  else
    offset = arr[ans_stamp - 1];

  std::cout << ans << '\n' << ans_l << '\n';
  for (int i = 0; i < ans_l; i++)
    std::cout << (int)(s[offset + i] - 'a') << ' ';
  std::cout << std::endl;
  return 0;
}
