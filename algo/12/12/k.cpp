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

template<typename OutT, typename InT>
std::vector<OutT> find_next_successor(const std::vector<InT> &s, bool from_end = false) {
  int begin = 0, end = (int)s.size(), delta = 1;
  if (from_end)
    begin = (int)(s.size() - 1), end = -1, delta = -1;
  std::vector<OutT> res(s.size(), (OutT)end);
  std::stack<OutT, std::vector<OutT>> unfilled;
  for (int i = begin; i != end; i += delta) {
    while (!unfilled.empty() && s[unfilled.top()] < s[i]) {
      res[unfilled.top()] = (OutT)i;
      unfilled.pop();
    }
    unfilled.push((OutT)i);
  }

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
  std::string s;
  std::cin >> s;
  int n = (int)s.size();

  auto suf_arr = find_suffix_array<short>(s);
  std::vector<int> suf_inv(suf_arr.size());
  {
    short i = 0;
    for (auto si : suf_arr) suf_inv[si] = i++;
  }
  auto lcp = buildLCP(s, suf_arr);
  auto lcp_rmq = sparse_table(lcp, [](auto &a, auto &b) { return std::min(a, b); });

  std::list<int> ordered_offsets(suf_arr.begin(), suf_arr.end());
  std::vector<std::list<int>::iterator> offset2iter(n);
  for (auto iter = ordered_offsets.begin(); iter != ordered_offsets.end(); ++iter)
    offset2iter[*iter] = iter;

  int q;
  std::cin >> q;

  struct query {
    short l, r;
    short i;
    int ans = 0;

    bool operator<(query other) const noexcept {
      return std::make_tuple(-r, l, i) < std::make_tuple(-other.r, other.l, other.i);
    }
  };
  std::vector<query> queries(q);
  for (int i = 0; i < q; i++) {
    std::cin >> queries[i].l >> queries[i].r;
    --queries[i].l, --queries[i].r;
    queries[i].i = i;
  }

  std::sort(queries.begin(), queries.end());

  int from = 0, to = n, ans = 0;

  std::list<int> local_ordered_offsets;
  std::vector<std::list<int>::iterator> local_offset2iter(n);

  std::vector<int> r2bias(n, 0);

  auto move_from = [&](int new_from) {
    while (from < new_from) {
      auto iter = local_offset2iter[from];
      int prev_v = 0, next_v = 0;
      if (iter != local_ordered_offsets.begin())
        prev_v = lcp_rmq.eval(suf_inv[*std::prev(iter)], suf_inv[*iter]);
      if (iter != --local_ordered_offsets.end())
        next_v = lcp_rmq.eval(suf_inv[*iter], suf_inv[*std::next(iter)]);
      ans -= std::max(to - from + 1 - std::max(prev_v, next_v), 0);
      local_ordered_offsets.erase(local_offset2iter[from++]);
    }
  };

  auto finish_to = [&]() {
    if (to != n) {
      move_from(to + 1);
      r2bias[to] = -ans;
    } else {
      to--;
    }
  };

  for (auto &q : queries) {
    if (to != q.r) {
      finish_to();

      while (to > q.r)
        ordered_offsets.erase(offset2iter[to--]);
      local_ordered_offsets = ordered_offsets;
      for (auto iter = local_ordered_offsets.begin(); iter != local_ordered_offsets.end(); ++iter)
        local_offset2iter[*iter] = iter;
      from = 0;
      ans = 0;
    }

    move_from(q.l);
    q.ans = ans;
  }
  finish_to();

  std::vector<int> res(q);
  for (auto q : queries)
    res[q.i] = q.ans + r2bias[q.r];

  for (auto a : res)
    std::cout << a << '\n';
  std::cout.flush();
  return 0;
}
