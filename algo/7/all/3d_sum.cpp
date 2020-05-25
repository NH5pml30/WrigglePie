/* Kholiavin Nikolai, M3138 */
#include <vector>
#include <iostream>

class bit_3d {
 private:
  std::vector<std::vector<std::vector<uint32_t>>> f;
  int w, l, h;

 public:
  bit_3d(int x, int y, int z) : f(x), w(x), l(y), h(z) {
    for (auto &v : f) {
      v.resize(y);
      for (auto &u : v)
        u.resize(z, 0);
    }
  }

  void add(int x, int y, int z, uint32_t val) {
    int i = x;
    while (i < w) {
      int j = y;
      while (j < l) {
        int k = z;
        while (k < h) {
          f[i][j][k] += val;
          k |= (k + 1);
        }
        j |= (j + 1);
      }
      i |= (i + 1);
    }
  }

  uint32_t get(int x, int y, int z) const {
    uint32_t ans = 0;

    int i = x;
    while (i >= 0) {
      int j = y;
      while (j >= 0) {
        int k = z;
        while (k >= 0) {
          ans += f[i][j][k];
          k = (k & (k + 1)) - 1;
        }
        j = (j & (j + 1)) - 1;
      }
      i = (i & (i + 1)) - 1;
    }

    return ans;
  }
};

class bit_rng_upd_pt_qry_3d {
 private:
  bit_3d diff;
  int w, l, h;

 public:
  bit_rng_upd_pt_qry_3d(int x, int y, int z)
      : diff(x, y, z), w(x), l(y), h(z) {
  }

  void add_range(int x1, int x2, int y1, int y2, int z1, int z2, uint32_t val) {
    diff.add(x1, y1, z1, val);
    if (x2 < w) diff.add(x2, y1, z1, 0 - val);
    if (y2 < l) diff.add(x1, y2, z1, 0 - val);
    if (z2 < h) diff.add(x1, y1, z2, 0 - val);
    if (x2 < w && y2 < l) diff.add(x2, y2, z1, val);
    if (y2 < l && z2 < h) diff.add(x1, y2, z2, val);
    if (x2 < w && z2 < h) diff.add(x2, y1, z2, val);
    if (x2 < w && y2 < l && z2 < h) diff.add(x2, y2, z2, 0 - val);
  }

  uint32_t get(int x, int y, int z) { return diff.get(x, y, z); }
};

class bit_rng_upd_rng_qry_3d {
 private:
  enum coef_id {
    C_CONST,
    C_X,
    C_Y,
    C_Z,
    C_XY,
    C_XZ,
    C_YZ,
    C_XYZ,
    C_size
  };
  bit_rng_upd_pt_qry_3d coefs[C_size];
  int w, l, h;

 public:
  bit_rng_upd_rng_qry_3d(int x, int y, int z)
      : coefs{{x, y, z}, {x, y, z}, {x, y, z}, {x, y, z}, {x, y, z},
              {x, y, z}, {x, y, z}, {x, y, z}}, w(x), l(y), h(z) {
  }

  void add_prefix(int x, int y, int z, uint32_t val) {
    // i < x && j < y && k < z: += (i+1)(j+1)(k+1)val
    coefs[C_XYZ].add_range(0, x, 0, y, 0, z, val);

    // i >= x && j < y && k < z: += x(j+1)(k+1)val
    coefs[C_YZ].add_range(x, w, 0, y, 0, z, val * x);
    // i < x && j >= y && k < z: += (i+1)y(k+1)val
    coefs[C_XZ].add_range(0, x, y, l, 0, z, val * y);
    // i < x && j < y && k >= z: += (i+1)(j+1)zval
    coefs[C_XY].add_range(0, x, 0, y, z, h, val * z);

    // i >= x && j >= y && k < z: += xy(k+1)val
    coefs[C_Z].add_range(x, w, y, l, 0, z, val * x * y);
    // i >= x && j < y && k >= z: += x(j+1)zval
    coefs[C_Y].add_range(x, w, 0, y, z, h, val * x * z);
    // i < x && j >= y && k >= z: += (i+1)yzval
    coefs[C_X].add_range(0, x, y, l, z, h, val * y * z);

    // i >= x && j >= y && k >= z: += xyzval
    coefs[C_CONST].add_range(x, w, y, l, z, h, val * x * y * z);
  }

  void add_range(int x1, int x2, int y1, int y2, int z1, int z2, uint32_t val) {
    // inclusion–exclusion principle
    add_prefix(x2, y2, z2, val);
    add_prefix(x1, y2, z2, 0 - val);
    add_prefix(x2, y1, z2, 0 - val);
    add_prefix(x2, y2, z1, 0 - val);

    add_prefix(x1, y1, z2, val);
    add_prefix(x1, y2, z1, val);
    add_prefix(x2, y1, z1, val);

    add_prefix(x1, y1, z1, 0 - val);
  }

  uint32_t sum_prefix(int x, int y, int z) {
    if (x == 0 || y == 0 || z == 0) return 0;

    int xc = x - 1, yc = y - 1, zc = z - 1;
    return coefs[C_XYZ].get(xc, yc, zc) * x * y * z +
        coefs[C_XY].get(xc, yc, zc) * x * y +
        coefs[C_YZ].get(xc, yc, zc) * y * z +
        coefs[C_XZ].get(xc, yc, zc) * x * z +
        coefs[C_X].get(xc, yc, zc) * x +
        coefs[C_Y].get(xc, yc, zc) * y +
        coefs[C_Z].get(xc, yc, zc) * z +
        coefs[C_CONST].get(xc, yc, zc);
  }

  uint32_t sum_range(int x1, int x2, int y1, int y2, int z1, int z2) {
    // inclusion–exclusion principle
    return sum_prefix(x2, y2, z2)
      - sum_prefix(x1, y2, z2)
      - sum_prefix(x2, y1, z2)
      - sum_prefix(x2, y2, z1)

      + sum_prefix(x1, y1, z2)
      + sum_prefix(x1, y2, z1)
      + sum_prefix(x2, y1, z1)

      - sum_prefix(x1, y1, z1);
  }
};

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int nx, ny, nz;
  std::cin >> nx >> ny >> nz;
  bit_rng_upd_rng_qry_3d tree(nx, ny, nz);

  int q;
  std::cin >> q;
  for (int i = 0; i < q; i++) {
    char command;
    int x1, y1, z1, x2, y2, z2;
    std::cin >> command >> x1 >> y1 >> z1 >> x2 >> y2 >> z2;
    uint32_t val;
    switch (command) {
      case 'a':
        std::cin >> val;
        tree.add_range(x1, x2, y1, y2, z1, z2, val);
        break;
      case 's':
        std::cout << tree.sum_range(x1, x2, y1, y2, z1, z2) << '\n';
        break;
    }
  }
  std::cout.flush();
  return 0;
}
