#pragma once

#include "token_visitor.h"

template<typename CRTP_child>
struct simple_token : token {
  using token::token;

  void accept(token_visitor &vis) override {
    static_assert(std::derived_from<CRTP_child, simple_token>);
    vis.visit(*static_cast<CRTP_child *>(this));
  }
};

struct number_token : simple_token<number_token> {
  int val;

  number_token(tok_pos pos, int val) noexcept : simple_token(pos), val(val) {}
};

struct brace_token : simple_token<brace_token> {
  bool is_closing;

  brace_token(tok_pos pos, bool is_closing) noexcept : simple_token(pos), is_closing(is_closing) {}
};

struct op_token : simple_token<op_token> {
  enum class op_type { add, sub, mul, div } type;

  op_token(tok_pos pos, op_type type) noexcept : simple_token(pos), type(type) {}
};
