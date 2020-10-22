#pragma once

#include <fstream>
#include <vector>
#include <functional>
#include <algorithm>
#include <numeric>
#include <optional>

namespace g6 {
  class storage {
   private:
    std::istream &input_stream;
    using byte = unsigned char;

    std::optional<byte> try_read_byte() {
      unsigned char ch;
      if (!(input_stream >> ch)) return {};
      if (!(ch >= 63 && ch <= 126))
        throw std::exception("Wrong format");
      return ch - 63;
    }

    byte read_byte() {
      auto b = try_read_byte();
      if (!b.has_value())
        throw std::exception("Unexpected eof");
      return b.value();
    }

    std::vector<bool> read_r(size_t k) {
      std::vector<bool> res;

      size_t n_bytes = (k + 5) / 6;
      for (size_t i = 0; i < n_bytes; i++) {
        byte next_byte = read_byte();
        for (size_t j = 0; j < 6 && k > 0; j++, k--)
          res.push_back((next_byte >> (5 - j)) & 1);
      }
      return res;
    }

    uint64_t vector2number(const std::vector<bool> &num) {
      return std::accumulate(num.begin(), num.end(), uint64_t{0},
                             [](uint64_t l, bool r) { return (l << 1) | r; });
    }

    uint64_t read_n() {
      auto bb = try_read_byte();
      if (!bb.has_value())
        return 0;
      byte b = bb.value();
      if (b == 63) {
        b = read_byte();
        if (b == 63) {
          return vector2number(read_r(36));
        } else {
          input_stream.putback(b + 63);
          return vector2number(read_r(18));
        }
      } else {
        return b;
      }
    }

   public:
    storage(std::istream &input_stream) : input_stream(input_stream) {}

    graph_t next_graph() {
      graph_t g;
      uint64_t n = read_n();
      if (input_stream.eof())
        return g;
      g.resize(n);
      std::vector<bool> mat = read_r(n * (n - 1) / 2);
      //char nl;
      //input_stream >> nl;
      size_t ind = 0;
      int edge_id = 0;
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < i; j++)
          if (mat[ind++]) {
            g.add_edge(i, j, {});
            g.add_edge(j, i, {});
            edge_id++;
          }
      return g;
    }
  };
}
