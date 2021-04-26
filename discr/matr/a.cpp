/* Kholiavin Nikolai, M3138 */
#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <set>
#include <vector>
#include <stack>
#include <memory>
#include <fstream>
#include <map>

template<class ChildT>
struct counting_iterator {
  using iterator_category = std::forward_iterator_tag;
  using value_type = int;
  using difference_type = int;
  using pointer = int *;
  using reference = int &;

  int cur = 0;

  counting_iterator(int begin = 0) : cur(begin) {}
  virtual void increment() {}
  virtual bool equals(const ChildT &other) const {
    return cur == static_cast<const counting_iterator &>(other).cur;
  }
  ChildT &operator++() {
    cur++;
    increment();
    return static_cast<ChildT &>(*this);
  }
  ChildT operator++(int) {
    auto res = ChildT(static_cast<const ChildT &>(*this));
    operator++();
    return res;
  }
  int operator*() const {
    return cur;
  }
  bool operator==(const counting_iterator &other) const {
    return equals(static_cast<const ChildT &>(other));
  }
  bool operator!=(const counting_iterator &other) const {
    return !operator==(other);
  }
  void reset(int new_cur) {
    cur = new_cur;
  }
};

struct counting_iterator_t : counting_iterator<counting_iterator_t> {
  using base_t = counting_iterator<counting_iterator_t>;
  using base_t::counting_iterator;
};

template <typename type>
class monoid {
 private:
  std::function<type(const type &, const type &)> func;
  type identity_element;

 public:
  monoid() {}
  monoid(const std::function<type(const type &, const type &)> &func,
         type identity_element)
      : func(func), identity_element(std::move(identity_element)) {}

  type operator()(const type &lhs, const type &rhs) const {
    if (lhs == identity_element) return rhs;
    if (rhs == identity_element) return lhs;
    return func(lhs, rhs);
  }

  const type &identity() const { return identity_element; }
};

template <typename type, typename ranged_op>
class segment_tree {
 private:
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

 private:
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
    if (l >= rx || lx >= r) return func.identity();
    if (lx >= l && rx <= r) return nodes[x];
    int m = (lx + rx) / 2;
    return func(eval_(l, r, 2 * x + 1, lx, m), eval_(l, r, 2 * x + 2, m, rx));
  }

 public:
  segment_tree(
      const std::vector<type> &a, const monoid<type> &func,
      const monoid<ranged_op> &op_compose,
      const std::function<type(const type &, const ranged_op &)> &op_apply)
      : func(func), op_compose(op_compose), op_apply(op_apply) {
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
        nodes[begin + i] =
            func(nodes[2 * (begin + i) + 1], nodes[2 * (begin + i) + 2]);
  }

  void apply(int l, int r, const ranged_op &op) {
    apply_(l, r, op, 0, 0, size);
  }

  type eval(int l, int r) { return eval_(l, r, 0, 0, size); }
};

template<class InputIt, class OutputIt, class ToNumber>
void integer_set_complement(int n, InputIt in_begin, InputIt in_end, OutputIt out_begin,
                            ToNumber func) {
  using number_t = std::decay_t<decltype(func(*in_begin))>;
  static_assert(std::is_integral_v<number_t>);
  std::vector<number_t> initial_vals;
  std::transform(in_begin, in_end, std::back_inserter(initial_vals), func);
  std::sort(initial_vals.begin(), initial_vals.end());
  std::set_difference(counting_iterator_t(), counting_iterator_t(n), initial_vals.begin(),
                      initial_vals.end(), out_begin);
}

int main()
{
#ifdef _DEBUG
  auto &in = std::cin;
  auto &out = std::cout;
#else
  auto in = std::ifstream("schedule.in");
  auto out = std::ofstream("schedule.out");
#endif

  int n;
  in >> n;
  struct task
  {
    int d, w, id;
  };
  std::vector<task> tasks(n);
  std::set<int> deadlines;
  for (int i = 0; i < n; i++)
  {
    int d, w;
    in >> d >> w;
    tasks[i] = {d, w, i};
    deadlines.insert(d);
  }
  std::vector<task> sorted = tasks;
  std::sort(sorted.begin(), sorted.end(),
            [](const task &left, const task &right) { return left.w > right.w; });

  std::map<int, int> deadline2id;
  std::vector<int> bias(deadlines.size());
  {
    int id = 0;
    for (auto el : deadlines)
    {
      deadline2id[el] = id;
      bias[id] = -el;
      id++;
    }
  }

  segment_tree<int, int> tree(bias, monoid<int>([](int x, int y) { return std::max(x, y); }, std::numeric_limits<int>::min()),
    monoid<int>(std::plus<int>(), 0), std::plus<int>());
  std::vector<int> chosen_tasks;

  for (auto &t : sorted)
  {
    int did = deadline2id[t.d];
    if (tree.eval(did, deadlines.size()) < 0)
    {
      tree.apply(did, deadlines.size(), +1);
      chosen_tasks.push_back(t.id);
    }
  }

  std::vector<int> skipped;
  integer_set_complement(n, chosen_tasks.begin(), chosen_tasks.end(), std::back_inserter(skipped),
                         [](int x) { return x; });

  long long fine = 0;
  for (auto t_id : skipped)
    fine += tasks[t_id].w;

  out << fine << std::endl;

  return 0;
}
