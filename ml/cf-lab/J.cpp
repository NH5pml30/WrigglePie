#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <functional>
#include <initializer_list>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <numbers>
#include <numeric>
#include <optional>
#include <random>
#include <ranges>
#include <set>
#include <span>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <valarray>
#include <variant>
#include <vector>


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
  std::unique_ptr<T[]> data;

  matrix() = default;

  matrix(int h, int w, std::ranges::range auto &&data) : h(h), w(w)
  {
    this->data = std::make_unique<T[]>(w * h);
    std::fill_n(this->data.get(), w * h, T(0));
    std::ranges::copy(data, this->data.get());
  }

  matrix(const matrix &other)
      : h(other.h), w(other.w), data(std::make_unique<T[]>(other.w * other.h))
  {
    std::ranges::copy_n(other.data.get(), w * h, this->data.get());
  }
  matrix(matrix &&) = default;
  matrix &operator=(const matrix &other)
  {
    h = other.h;
    w = other.w;
    data = other.data;
  }
  matrix &operator=(matrix &&) = default;

  T &operator()(int i, int j)
  {
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
                      std::views::transform([w, generator = std::move(generator)](int i) {
                        return std::views::iota(0, w) |
                               std::views::transform([i, generator = std::move(generator)](int j) {
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
    std::transform(data.get(), data.get() + h * w, rhs.data.get(), data.get(), std::plus<>{});
    return *this;
  }

  matrix &operator&=(const matrix &rhs)
  {
    assert(w == rhs.w && h == rhs.h);
    std::transform(data.get(), data.get() + h * w, rhs.data.get(), data.get(), std::multiplies<>{});
    return *this;
  }

  friend std::ostream &operator<<(std::ostream &o, const matrix &self)
  {
    std::ranges::for_each(std::views::iota(0, self.h), [&self, &o](int i) {
      std::ranges::for_each(self.crow(i), [&o](double val) { o << std::setprecision(10) << val << ' '; });
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
    VAR,
    TNH,
    SGM,
    MUL,
    SUM,
    HAD
  };

  Kind v_kind;
  std::vector<int> deps;
  using count_dims_t =
      std::function<std::pair<int, int>(const std::vector<std::unique_ptr<Vertex>> &)>;
  count_dims_t count_dims;
  matrix_t cached_output{};
  matrix_t der_wrt_output{};

  Vertex(Kind v_kind, std::vector<int> deps, count_dims_t count_dims)
      : v_kind(v_kind), deps(std::move(deps)), count_dims(std::move(count_dims))
  {
  }

  virtual matrix_t compute(const std::vector<std::unique_ptr<Vertex>> &) = 0;

  virtual void forward(const std::vector<std::unique_ptr<Vertex>> &verts)
  {
    cached_output = compute(verts);
    der_wrt_output = matrix_t(cached_output.h, cached_output.w, std::vector<double>{});
  }

  void back_propagate(const std::vector<std::unique_ptr<Vertex>> &verts)
  {
    for (int i = 0; i < (int)deps.size(); i++)
      verts[deps[i]]->der_wrt_output += back_propagate_impl(verts, i);
  }

  virtual matrix_t back_propagate_impl(const std::vector<std::unique_ptr<Vertex>> &verts, int dep_i) = 0;

  virtual ~Vertex() {}
};

struct VarVertex : Vertex
{
  VarVertex(int r, int c) : Vertex(Kind::VAR, {}, [r, c](auto &&) { return std::make_pair(r, c); })
  {
  }

  void set(matrix_t data)
  {
    cached_output = std::move(data);
  }

  matrix_t compute(const std::vector<std::unique_ptr<Vertex>> &) override
  {
    throw "unreachable";
  }

  void forward(const std::vector<std::unique_ptr<Vertex>> &) override
  {
    der_wrt_output = matrix_t(cached_output.h, cached_output.w, std::vector<double>{});
  }

  matrix_t back_propagate_impl(const std::vector<std::unique_ptr<Vertex>> &verts, int dep_i)
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
           std::views::transform(
               [this, &verts, i, j](int v_i) { return verts[deps[v_i]]->cached_output(i, j); });
  }

  matrix_t compute(const std::vector<std::unique_ptr<Vertex>> &verts) override
  {
    auto &m = verts[deps[0]]->cached_output;
    return matrix_t::generate(m.h, m.w, [this, &verts](int i, int j) {
      return static_cast<CRTPChild *>(this)->op_impl(get_ins(verts, i, j));
    });
  }

  matrix_t back_propagate_impl(const std::vector<std::unique_ptr<Vertex>> &verts, int dep_i) override
  {
    return matrix_t::generate(
        der_wrt_output.h, der_wrt_output.w, [this, &verts, dep_i](int i, int j) {
          return der_wrt_output(i, j) *
                 static_cast<CRTPChild *>(this)->grad_impl(get_ins(verts, i, j), dep_i);
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
  double unary_grad_impl(double in)
  {
    return 1 / sqr(std::cosh(in));
  }
  double unary_op_impl(double in)
  {
    return tanh(in);
  }
};

struct SigmoidVertex : UnaryElementWiseVertex<SigmoidVertex, Vertex::Kind::SGM>
{
  using UnaryElementWiseVertex<SigmoidVertex, Vertex::Kind::SGM>::UnaryElementWiseVertex;

  double unary_grad_impl(double in)
  {
    double ex = std::exp(-in);
    return ex / sqr(1 + ex);
  }
  double unary_op_impl(double in)
  {
    return 1 / (1 + std::exp(-in));
  }
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
    return verts[deps[0]]->cached_output * verts[deps[1]]->cached_output;
  }

  matrix_t back_propagate_impl(const std::vector<std::unique_ptr<Vertex>> &verts, int dep_i) override
  {
    if (dep_i == 0)
      return der_wrt_output * verts[deps[1]]->cached_output.transpose();
    else
      return verts[deps[0]]->cached_output.transpose() * der_wrt_output;
  }
};

struct SumVertex : ElementWiseVertex<SumVertex, Vertex::Kind::SUM>
{
  using ElementWiseVertex<SumVertex, Vertex::Kind::SUM>::ElementWiseVertex;

  double grad_impl(std::ranges::random_access_range auto &&, int)
  {
    return 1;
  }
  double op_impl(std::ranges::random_access_range auto &&ins)
  {
    return fold(ins, 0.0);
  }

  matrix_t back_propagate_impl(const std::vector<std::unique_ptr<Vertex>> &verts, int dep_i) override
  {
    return der_wrt_output;
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

struct EvalGraph
{
  std::vector<std::unique_ptr<Vertex>> verts;

  int add(std::unique_ptr<Vertex> v)
  {
    int res = (int)verts.size();
    verts.push_back(std::move(v));
    return res;
  }

  void forward()
  {
    for (auto &v : verts)
      v->forward(verts);
  }

  void back_propagate()
  {
    for (auto &v : std::views::reverse(verts))
      v->back_propagate(verts);
  }
};

struct LSTM
{
  struct LSTM_WUb
  {
    int Wi, Ui, bi;

    LSTM_WUb(EvalGraph &graph, matrix_t W, matrix_t U, matrix_t b)
    {
      int N = b.h;
      Wi = graph.add(std::make_unique<VarVertex>(W.h, W.w));
      static_cast<VarVertex &>(*graph.verts[Wi]).set(W);
      Ui = graph.add(std::make_unique<VarVertex>(U.h, U.w));
      static_cast<VarVertex &>(*graph.verts[Ui]).set(U);
      bi = graph.add(std::make_unique<VarVertex>(b.h, b.w));
      static_cast<VarVertex &>(*graph.verts[bi]).set(b);
    }

    int add(EvalGraph &graph, int xi, int hi)
    {
      int wx = graph.add(std::make_unique<MulVertex>(Wi, xi));
      int uh = graph.add(std::make_unique<MulVertex>(Ui, hi));
      return graph.add(std::make_unique<SumVertex>(std::vector<int>{wx, uh, bi}));
    }
  };

  EvalGraph graph;
  LSTM_WUb f, i, o, c;
  std::vector<int> os;
  int h0 = -1, c0 = -1, hm, cm;
  std::vector<int> xs;

  LSTM(matrix_t wf, matrix_t uf, matrix_t bf, matrix_t wi, matrix_t ui, matrix_t bi, matrix_t wo,
       matrix_t uo, matrix_t bo, matrix_t wc, matrix_t uc, matrix_t bc, matrix_t h, matrix_t c)
      : f(graph, wf, uf, bf), i(graph, wi, ui, bi), o(graph, wo, uo, bo), c(graph, wc, uc, bc)
  {
    int N = h.h;
    hm = h0 = graph.add(std::make_unique<VarVertex>(N, 1));
    static_cast<VarVertex &>(*graph.verts[h0]).set(h);
    cm = c0 = graph.add(std::make_unique<VarVertex>(N, 1));
    static_cast<VarVertex &>(*graph.verts[c0]).set(c);
  }

  void add(matrix_t x)
  {
    int N = x.h;
    int xi = graph.add(std::make_unique<VarVertex>(N, 1));
    static_cast<VarVertex &>(*graph.verts[xi]).set(std::move(x));
    int oi = graph.add(std::make_unique<SigmoidVertex>(o.add(graph, xi, hm)));
    int ci = graph.add(std::make_unique<SumVertex>(std::vector{
      graph.add(std::make_unique<HadVertex>(std::vector{
        graph.add(std::make_unique<SigmoidVertex>(f.add(graph, xi, hm))),
        cm
      })),
      graph.add(std::make_unique<HadVertex>(std::vector{
        graph.add(std::make_unique<SigmoidVertex>(i.add(graph, xi, hm))),
        graph.add(std::make_unique<TnhVertex>(c.add(graph, xi, hm)))
      }))
    }));
    int hi = graph.add(std::make_unique<HadVertex>(std::vector{oi, ci}));
    os.push_back(oi);
    xs.push_back(xi);
    hm = hi;
    cm = ci;
  }

  void forward()
  {
    graph.forward();
  }

  void back_propagate()
  {
    graph.back_propagate();
  }
};

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

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


  int N = read_int();

  auto read_vector_N = [&do_read_vector_int, N]() { return matrix_t(N, 1, do_read_vector_int(N)); };

  matrix_t ms_[12];
  for (int i = 0; i < 12; i++)
    if (i % 3 == 2)
      ms_[i] = read_vector_N();
    else
      ms_[i] = matrix_t(N, N, do_read_vector_int(N * N));

  #define ms std::move(ms_)

  int M = read_int();

  matrix_t h0 = read_vector_N(), c0 = read_vector_N();
  LSTM network(ms[0], ms[1], ms[2], ms[3], ms[4], ms[5], ms[6], ms[7], ms[8], ms[9], ms[10], ms[11],
               h0, c0);
  for (int i = 0; i < M; i++)
    network.add(read_vector_N());
  network.forward();

  network.graph.verts[network.hm]->der_wrt_output = read_vector_N();
  network.graph.verts[network.cm]->der_wrt_output = read_vector_N();
  for (int oi : std::views::reverse(network.os))
    network.graph.verts[oi]->der_wrt_output = read_vector_N();

  network.back_propagate();

  for (int oi : network.os)
    std::cout << network.graph.verts[oi]->cached_output;
  std::cout << network.graph.verts[network.hm]->cached_output;
  std::cout << network.graph.verts[network.cm]->cached_output;
  for (int xi : std::views::reverse(network.xs))
    std::cout << network.graph.verts[xi]->der_wrt_output;
  std::cout << network.graph.verts[network.h0]->der_wrt_output;
  std::cout << network.graph.verts[network.c0]->der_wrt_output;

  std::cout << network.graph.verts[network.f.Wi]->der_wrt_output;
  std::cout << network.graph.verts[network.f.Ui]->der_wrt_output;
  std::cout << network.graph.verts[network.f.bi]->der_wrt_output;
  std::cout << network.graph.verts[network.i.Wi]->der_wrt_output;
  std::cout << network.graph.verts[network.i.Ui]->der_wrt_output;
  std::cout << network.graph.verts[network.i.bi]->der_wrt_output;
  std::cout << network.graph.verts[network.o.Wi]->der_wrt_output;
  std::cout << network.graph.verts[network.o.Ui]->der_wrt_output;
  std::cout << network.graph.verts[network.o.bi]->der_wrt_output;
  std::cout << network.graph.verts[network.c.Wi]->der_wrt_output;
  std::cout << network.graph.verts[network.c.Ui]->der_wrt_output;
  std::cout << network.graph.verts[network.c.bi]->der_wrt_output;

  return 0;
}
