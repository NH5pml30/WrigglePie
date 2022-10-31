#pragma once

#include <vector>
#include <memory>
#include <string>
#include <istream>

#include "parser_exception.h"
#include "tokens.h"

class tokenizer : public errorable_state_machine_base<tokenizer> {
  friend class errorable_state_machine_base<tokenizer>;
public:
  std::unique_ptr<token> next_token();

  tokenizer(std::istream &is) : is(&is) {
    is.exceptions(is.badbit);
  }

private:
  std::istream *is;
  std::string cur_line_data;
  std::string_view cur_line;
  int line_no = -1;

  std::unique_ptr<token> cur_token;

  bool next_line();
  char peek_next_char();
  char peek_next_nonws_char();
  void advance(size_t delta = 1);
  tok_pos get_pos() const;
  parser_exception create_exception(const std::string &msg);
  [[noreturn]] void fail(const std::string &msg);
  [[noreturn]] void fail_with_nested(const std::string &msg);

  template<typename TokenT, typename... ArgsT>
  std::unique_ptr<TokenT> create_token(ArgsT &&...args) {
    return std::make_unique<TokenT>(get_pos(), std::forward<ArgsT>(args)...);
  }

  enum class tok_state {
    prefix,
    suffix,
    error,
    end
  } state = tok_state::prefix;

  using state_t = tok_state;
};
