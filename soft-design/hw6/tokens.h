#pragma once

#include <string>
#include <format>

struct tok_pos {
  int line_no, col_no;

  static tok_pos end_pos() noexcept {
    return tok_pos{-1, -1};
  }

  bool operator==(const tok_pos &) const = default;

  std::string to_string() const {
    return *this == end_pos() ? "<end>" : std::format("{}:{}", line_no, col_no);
  }
};

class token_visitor;

struct token {
  tok_pos pos;

  token(tok_pos pos) noexcept : pos(pos) {}

  virtual void accept(token_visitor &vis) = 0;

  virtual ~token() = default;
};

struct number_token;
struct brace_token;
struct op_token;
