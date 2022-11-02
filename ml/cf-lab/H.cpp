#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <cmath>
#include <numbers>
#include <numeric>
#include <random>
#include <ranges>
#include <set>
#include <span>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>
#include <initializer_list>
#include <optional>
#include <cassert>
#include <memory>

auto fold(std::ranges::range auto &&range)
{
  return std::reduce(std::begin(range), std::end(range));
}

template<typename Bop = std::plus<>>
auto fold(std::ranges::range auto &&range, auto init, Bop bop = {})
{
  return std::reduce(std::begin(range), std::end(range), std::move(init), std::move(bop));
}

template<typename T, typename ReduceOp = std::plus<>, typename BinOp = std::multiplies<>>
auto inner_product(std::ranges::range auto &&r1, std::ranges::range auto &&r2, T init = T{},
                   ReduceOp rop = {}, BinOp bop = {})
{
  return std::inner_product(r1.begin(), r1.end(), r2.begin(), init, std::move(rop), std::move(bop));
}

template<template<typename> typename T = std::vector>
auto materialize(std::ranges::range auto &&range)
{
  auto r = range | std::views::common;
  return T<std::ranges::range_value_t<decltype(r)>>(std::begin(r), std::end(r));
}

auto sqr(auto x)
{
  return x * x;
}

template<typename T>
struct matrix
{
  int h, w;
  std::shared_ptr<T[]> data;

  matrix() = default;

  matrix(int h, int w, std::ranges::range auto &&data) : h(h), w(w)
  {
    this->data = std::make_unique<T[]>(w * h);
    std::fill_n(this->data.get(), w * h, T(0));
    std::ranges::copy(data, this->data.get());
  }

  matrix(const matrix &other) : h(other.h), w(other.w), data(other.data)
  {
  }
  matrix(matrix &&) = default;
  matrix &operator=(const matrix &other)
  {
    h = other.h;
    w = other.w;
    data = other.data;
  }
  matrix &operator=(matrix &&) = default;

  void unshare()
  {
    if (data.use_count() > 1)
    {
      auto saved = std::move(data);
      data = std::make_unique<T[]>(w * h);
      std::copy_n(saved.get(), w * h, data.get());
    }
  }

  T &operator()(int i, int j)
  {
    unshare();
    return data[i * w + j];
  }

  const T &operator()(int i, int j) const
  {
    return data[i * w + j];
  }

  auto row(int i) const
  {
    return std::views::iota(0, w) |
           std::views::transform([this, i](int ind) { return data[i * w + ind]; });
  }
  auto col(int i) const
  {
    return std::views::iota(0, h) |
           std::views::transform([this, i](int ind) { return data[ind * w + i]; });
  }
  auto crow(int i) const
  {
    return row(i);
  }
  auto ccol(int i) const
  {
    return col(i);
  }
  auto row(int i)
  {
    return std::views::iota(0, w) |
           std::views::transform([this, i](int ind) { return data[i * w + ind]; });
  }
  auto col(int i)
  {
    return std::views::iota(0, h) |
           std::views::transform([this, i](int ind) { return data[ind * w + i]; });
  }

  static matrix generate(int h, int w, auto generator)
  {
    return matrix(h, w,
                  std::views::iota(0, h) |
                      std::views::transform([w, &generator](int i) {
                        return std::views::iota(0, w) |
                               std::views::transform([i, &generator](int j) {
                                 return generator(i, j);
                               });
                      }) |
                      std::views::join);
  }

  matrix operator*(const matrix &other) const
  {
    assert(w == other.h);
    return generate(h, other.w, [this, &other](int i, int j) {
      return inner_product<double>(row(i), other.col(j));
    });
  }

  matrix transpose() const
  {
    return matrix(w, h,
                  std::views::iota(0, w) |
                      std::views::transform(std::bind(&matrix::ccol, this, std::placeholders::_1)) |
                      std::views::join);
  }

  matrix &operator+=(const matrix &rhs)
  {
    assert(w == rhs.w && h == rhs.h);
    unshare();
    std::transform(data.get(), data.get() + h * w, rhs.data.get(), data.get(),
                   [](double lhs, double rhs) { return lhs + rhs; });
    return *this;
  }

  friend std::ostream &operator<<(std::ostream &o, const matrix &self)
  {
    std::ranges::for_each(std::views::iota(0, self.h), [&self, &o](int i) {
      std::ranges::for_each(self.crow(i), [&o](double val) { o << val << ' '; });
      o << '\n';
    });
    return o;
  }
};

using matrix_t = matrix<double>;

struct Vertex
{
  enum class Kind
  {
    VAR, TNH, RLU, MUL, SUM, HAD
  };

  Kind v_kind;
  std::vector<int> deps;
  using count_dims_t = std::function<std::pair<int, int>(const std::vector<std::unique_ptr<Vertex>> &)>;
  count_dims_t count_dims;
  std::optional<matrix_t> cached_output{};

  Vertex(Kind v_kind, std::vector<int> deps, count_dims_t count_dims)
      : v_kind(v_kind), deps(std::move(deps)), count_dims(std::move(count_dims))
  {
  }

  virtual matrix_t compute(const std::vector<std::unique_ptr<Vertex>> &) = 0;

  virtual void operator()(const std::vector<std::unique_ptr<Vertex>> &verts)
  {
    cached_output = compute(verts);
  }

  virtual matrix_t
      back_propagate(const matrix_t &out, const std::vector<std::unique_ptr<Vertex>> &verts,
                     int dep_i) = 0;

  virtual ~Vertex() {}
};

struct VarVertex : Vertex
{
  matrix_t data;

  VarVertex(int r, int c) : Vertex(Kind::VAR, {}, [r, c](auto &&) { return std::make_pair(r, c); }) {}

  void set(matrix_t data)
  {
    this->cached_output = std::move(data);
  }

  matrix_t compute(const std::vector<std::unique_ptr<Vertex>> &) override
  {
    throw "unreachable";
  }

  void operator()(const std::vector<std::unique_ptr<Vertex>> &) override
  {
  }

  matrix_t back_propagate(const matrix_t &out, const std::vector<std::unique_ptr<Vertex>> &verts,
                          int dep_i)
  {
    throw "up";
  }
};

template<typename CRTPChild, Vertex::Kind v_kind>
struct ElementWiseVertex : Vertex
{
  template<typename... Ts>
  ElementWiseVertex(std::vector<int> deps)
      : Vertex(v_kind, std::move(deps),
               [i = deps[0]](auto &verts) { return verts[i]->count_dims(verts); })
  {
  }

  auto get_ins(const std::vector<std::unique_ptr<Vertex>> &verts, int i, int j)
  {
    return std::views::iota(0, (int)deps.size()) |
           std::views::transform([this, &verts, i, j](int v_i) {
             return (*verts[deps[v_i]]->cached_output)(i, j);
           });
  }

  matrix_t compute(const std::vector<std::unique_ptr<Vertex>> &verts) override
  {
    auto &m = *verts[deps[0]]->cached_output;
    return matrix_t::generate(m.h, m.w, [this, &verts](int i, int j) {
      return static_cast<CRTPChild *>(this)->op_impl(get_ins(verts, i, j));
    });
  }

  matrix_t back_propagate(const matrix_t &out, const std::vector<std::unique_ptr<Vertex>> &verts,
                          int dep_i) override
  {
    return matrix_t::generate(out.h, out.w, [this, &out, &verts, dep_i](int i, int j) {
      return out(i, j) * static_cast<CRTPChild *>(this)->grad_impl(get_ins(verts, i, j), dep_i);
    });
  }
};

template<typename CRTPChild, Vertex::Kind v_kind>
struct UnaryElementWiseVertex : ElementWiseVertex<CRTPChild, v_kind>
{
  template<typename... Ts>
  UnaryElementWiseVertex(int x) : ElementWiseVertex<CRTPChild, v_kind>({x})
  {
  }

  double op_impl(std::ranges::random_access_range auto &&ins)
  {
    return static_cast<CRTPChild *>(this)->unary_op_impl(ins[0]);
  }

  double grad_impl(std::ranges::random_access_range auto &&ins, int)
  {
    return static_cast<CRTPChild *>(this)->unary_grad_impl(ins[0]);
  }
};

struct TnhVertex : UnaryElementWiseVertex<TnhVertex, Vertex::Kind::TNH>
{
  using UnaryElementWiseVertex<TnhVertex, Vertex::Kind::TNH>::UnaryElementWiseVertex;
  double unary_grad_impl(double in) { return 1 / sqr(std::cosh(in)); }
  double unary_op_impl(double in) { return tanh(in); }
};

struct RLUVertex : UnaryElementWiseVertex<RLUVertex, Vertex::Kind::RLU>
{
  int alpha_inv;

  RLUVertex(int alpha_inv, int x)
      : alpha_inv(alpha_inv), UnaryElementWiseVertex<RLUVertex, Vertex::Kind::RLU>(x)
  {
  }

  double unary_grad_impl(double in) { return in < 0 ? 1.0 / alpha_inv : 1; }
  double unary_op_impl(double in) { return in < 0 ? in / alpha_inv : in; }
};

struct MulVertex : Vertex
{
  MulVertex(int a, int b)
      : Vertex(Kind::MUL, {a, b}, [a, b](auto &verts) {
          return std::make_pair(verts[a]->count_dims(verts).first,
                                verts[b]->count_dims(verts).second);
        })
  {
  }

  matrix_t compute(const std::vector<std::unique_ptr<Vertex>> &verts)
  {
    return *verts[deps[0]]->cached_output * *verts[deps[1]]->cached_output;
  }

  matrix_t back_propagate(const matrix_t &out, const std::vector<std::unique_ptr<Vertex>> &verts,
                          int dep_i) override
  {
    if (dep_i == 0)
      return out * verts[deps[1]]->cached_output->transpose();
    else
      return verts[deps[0]]->cached_output->transpose() * out;
  }
};

struct SumVertex : ElementWiseVertex<SumVertex, Vertex::Kind::SUM>
{
  using ElementWiseVertex<SumVertex, Vertex::Kind::SUM>::ElementWiseVertex;

  double grad_impl(std::ranges::random_access_range auto &&, int) { return 1; }
  double op_impl(std::ranges::random_access_range auto &&ins) { return fold(ins, 0.0); }

  matrix_t back_propagate(const matrix_t &out, const std::vector<std::unique_ptr<Vertex>> &verts,
                          int dep_i) override
  {
    return out;
  }
};

struct HadVertex : ElementWiseVertex<HadVertex, Vertex::Kind::HAD>
{
  using ElementWiseVertex<HadVertex, Vertex::Kind::HAD>::ElementWiseVertex;

  double grad_impl(std::ranges::random_access_range auto &&ins, int dep_i)
  {
    return fold(ins | std::ranges::views::take(dep_i), 1.0, std::multiplies<>{}) *
           fold(ins | std::ranges::views::drop(dep_i + 1), 1.0, std::multiplies<>{});
  }
  double op_impl(std::ranges::random_access_range auto &&ins)
  {
    return fold(ins, 1.0, std::multiplies<>{});
  }
};

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int noof_verts, noof_inputs, noof_outputs;
  std::cin >> noof_verts >> noof_inputs >> noof_outputs;

  auto read_int = [] {
    int i;
    std::cin >> i;
    return i;
  };
  auto read_dep = [&read_int] { return read_int() - 1; };
  auto do_read_vector_int = [](int n, int delta = 0) {
    std::vector<int> vals(n);
    for (auto &val : vals)
    {
      std::cin >> val;
      val += delta;
    }
    return vals;
  };
  auto read_vector_int = [&do_read_vector_int, &read_int](int delta = 0) {
    return do_read_vector_int(read_int(), delta);
  };

  std::vector<std::unique_ptr<Vertex>> verts;
  for (int i = 0; i < noof_verts; i++)
  {
    std::string type;
    std::cin >> type;
    std::unique_ptr<Vertex> v;
    if (type == "var")
    {
      int r = read_int();
      v = std::make_unique<VarVertex>(r, read_int());
    }
    else if (type == "tnh")
      v = std::make_unique<TnhVertex>(read_dep());
    else if (type == "rlu")
    {
      int a = read_int();
      v = std::make_unique<RLUVertex>(a, read_dep());
    }
    else if (type == "mul")
    {
      int a = read_dep();
      v = std::make_unique<MulVertex>(a, read_dep());
    }
    else if (type == "sum")
      v = std::make_unique<SumVertex>(read_vector_int(-1));
    else /* if (type == "had") */
      v = std::make_unique<HadVertex>(read_vector_int(-1));

    verts.push_back(std::move(v));
  }

  for (int i = 0; i < noof_inputs; i++)
  {
    VarVertex *v = dynamic_cast<VarVertex *>(verts[i].get());
    auto [r, c] = v->count_dims(verts);
    v->set(matrix_t(r, c, do_read_vector_int(r * c)));
  }

  // forwards
  for (auto &v : verts)
    (*v)(verts);

  std::vector<matrix_t> ders =
      materialize(std::views::iota(0, noof_verts - noof_outputs) | std::views::transform([&verts](int i) {
                    auto [h, w] = verts[i]->count_dims(verts);
                    return matrix_t(h, w, std::vector<double>{});
                  }));
  for (int i = 0; i < noof_outputs; i++)
  {
    // read possibly empty derivative?
    Vertex &v = *verts[noof_verts - noof_outputs + i];
    auto [r, c] = v.count_dims(verts);
    ders.emplace_back(r, c, do_read_vector_int(r * c));
  }
  for (int vi = noof_verts - 1; vi >= 0; vi--)
    for (int dep_i = 0; dep_i < (int)verts[vi]->deps.size(); dep_i++)
      ders[verts[vi]->deps[dep_i]] += verts[vi]->back_propagate(ders[vi], verts, dep_i);

  for (int i = 0; i < noof_outputs; i++)
    std::cout << *verts[noof_verts - noof_outputs + i]->cached_output;
  for (int i = 0; i < noof_inputs; i++)
    std::cout << ders[i];
  std::cout.flush();
  return 0;
}
