/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>
#include <random>
#include <tuple>
#include <cassert>
#include <random>

template<typename type>
std::tuple<type, type, type> euclid_(type a, type b) {
  if (b == 0)
    return {a, 1, 0};
  else {
    auto [d, x_, y_] = euclid_(b, a % b);
    return {d, y_, x_ - a / b * y_};
  }
}

template<typename type>
std::tuple<type, type, type> euclid(type a, type b) {
  if (a < b) {
    auto [d, x, y] = euclid_(b, a);
    return {d, y, x};
  } else {
    return euclid_(a, b);
  }
}

template<typename type>
constexpr type infty = std::numeric_limits<type>::max();

template<typename type>
type phi(type n) {
  type phi = n;
  for (type p = 2; p <= n / p; p++) {
    while (n % p == 0) {
      phi -= phi / p;
      n /= p;
    }
  }
  if (n > 1)
    phi -= phi / n;
  return phi;
}

namespace modular {

template<typename type>
type mod(type a, type mod) {
  return (a % mod + mod) % mod;
}

int inv(int n, int mod) {
  auto [d, x, y] = euclid(n, mod);
  (void)y;
  assert(d == 1);
  return modular::mod(x, mod);
}

template<typename type>
int last_bit(type x) {
  for (int i = sizeof(x) * CHAR_BIT - 1; i >= 0; i--)
    if (((x >> i) & 1) != 0)
      return i;
  return -1;
}

int64_t mul(int64_t a, int64_t b, int64_t mod) {
  int64_t res = 0;
  for (int i = last_bit(b); i >= 0; i--) {
    res = (res * 2) % mod;
    if (((b >> i) & 1) != 0)
      res = (res + a) % mod;
  }
  return res;
}

int64_t pow(int64_t b, int64_t p, int64_t mod) {
  int64_t res = 1;
  for (int i = last_bit(p); i >= 0; i--) {
    res = mul(res, res, mod);
    if (((p >> i) & 1) != 0)
      res = mul(b, res, mod);
  }
  return res;
}

int64_t a_log(int64_t a, int64_t mod) {
  int64_t pow0 = 0, pow1 = 1, val0 = 1, val1 = a;
  while (val1 != 1) {
    pow0 = pow1;
    val0 = val1;
    pow1 *= 2;
    val1 = modular::mul(val1, val1, mod);
  }

  int64_t L = pow0, R = pow1;
  while (L < R - 1) {
    int64_t M = (L + R) / 2;
    if (modular::pow(a, M, mod) == 1)
      R = M;
    else
      L = M;
  }
  return R;
}

int64_t get_log_prime_(int64_t a, int64_t b, int64_t mod, int64_t phi) {
  // z == b^alpha * a^beta
  struct state_t {
    int64_t alpha, beta, z;

    state_t(int64_t alpha, int64_t beta, int64_t a, int64_t b, int64_t mod, int64_t phi) :
      alpha(alpha), beta(beta), z(modular::mul(modular::pow(b, alpha, phi), modular::pow(a, beta, phi), mod)) {}

    state_t &next_state(int64_t a, int64_t b, int64_t mod, int64_t phi) {
      switch (z % 3) {
        case 0:
          alpha = (alpha + 1) % phi;
          z = modular::mul(b, z, mod);
          break;
        case 1:
          alpha = (alpha * 2) % phi;
          beta = (beta * 2) % phi;
          z = modular::mul(z, z, mod);
          break;
        case 2:
          beta = (beta + 1) % phi;
          z = modular::mul(a, z, mod);
      }
      return *this;
    }
  };

  auto create_state = [&, engine = std::default_random_engine{},
                       distr = std::uniform_int_distribution<int64_t>(0, phi - 1)]() mutable {
    return state_t(distr(engine), distr(engine), a, b, mod, phi);
  };

  auto state_i = create_state(), state_2i = state_i;

  while (true) {
    state_i.next_state(a, b, mod, phi);
    state_2i.next_state(a, b, mod, phi).next_state(a, b, mod, phi);
    if (state_i.z == state_2i.z) {
      // b^alpha_2i * a^beta_2i === b^alpha_i * a^beta_i
      // b^(alpha_2i-alpha_i) === a^(beta_i-beta_2i)
      // let l: l * (alpha_2i-alpha_i) === 1 (mod phi(n))
      //   also a^y === 1 (mod n) if y === 1 (mod phi(n))
      // then b === a^(l(beta_i-beta_2i)) <=>
      //   b^(alpha_2i-alpha_i) === a^(beta_i-beta^2i) (mod n)
      // and l*(beta_i-beta_2i) is the answer
      auto [d, l, y] = euclid<int64_t>(modular::mod(state_2i.alpha - state_i.alpha, phi), phi);
      if (d != 1)
        return -1;
      l = (l % phi + phi) % phi;
      return modular::mul(l, modular::mod(state_i.beta - state_2i.beta, phi), phi);
    }
  }
}

int64_t log_prime(int64_t a, int64_t b, int64_t p, int T) {
  // Pollard's rho algorithm
  int64_t phi = ::phi(p);
  for (int i = 0; i < T; i++) {
    int64_t res = get_log_prime_(a, b, p, phi);
    if (res < 0)
      continue;
    return res;
  }
  return -1;
}

} // namespace modular

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int a, b, n;
  std::cin >> a >> b >> n;
  std::cout << modular::log_prime(a, b, n, 100) << std::endl;
  return 0;
}

