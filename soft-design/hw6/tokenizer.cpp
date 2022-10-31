#include <string>
#include <format>

#include "tokenizer.h"
#include "grammar.h"

bool tokenizer::next_line() {
  try {
    bool res = true;
    do {
      cur_line_data.clear();
      res &= !!std::getline(*is, cur_line_data);
      line_no++;
    } while (res && cur_line_data.empty());
    cur_line_data.push_back('\n');
    cur_line = std::string_view(cur_line_data);
    return res;
  } catch (std::ios_base::failure &) {
    fail_with_nested("IO exception occured");
  }
}

char tokenizer::peek_next_char() {
  if (cur_line.empty() && !next_line())
    return 0;

  return cur_line.front();
}

char tokenizer::peek_next_nonws_char() {
  while (true) {
    char ch = peek_next_char();
    if (ch == 0 || !std::isspace(static_cast<unsigned char>(ch)))
      return ch;

    advance();
  }
}

void tokenizer::advance(size_t delta) {
  cur_line.remove_prefix(delta);
}

tok_pos tokenizer::get_pos() const {
  return tok_pos{.line_no = line_no + 1,
                 .col_no = static_cast<int>(cur_line.data() - cur_line_data.data() + 1)};
}

parser_exception tokenizer::create_exception(const std::string &msg) {
  return parser_exception(msg, get_pos());
}

[[noreturn]] void tokenizer::fail(const std::string &msg) {
  __super::fail(create_exception(msg));
}

[[noreturn]] void tokenizer::fail_with_nested(const std::string &msg) {
  __super::fail_with_nested(create_exception(msg));
}

int to_digit(char ch) {
  return ch - '0';
}

std::unique_ptr<token> tokenizer::next_token() {
  int accum = 0;
  while (true) {
    switch (state) {
    case tok_state::prefix: {
      char ch = peek_next_nonws_char();
      if (ch == 0) {
        state = tok_state::end;
        return {};
      } else if (ch == '(' || ch == ')') {
        advance();
        return create_token<brace_token>(ch == ')');
      } else if (std::isdigit(static_cast<unsigned char>(ch))) {
        accum = 0;
        state = tok_state::suffix;
        break;
      } else {
        auto opt_type = grammar_utils::match_and_advance(cur_line);
        if (opt_type)
          return create_token<op_token>(*opt_type);
        fail(std::format("Expected brace, operation or digit, found '{}'", ch));
      }
    }
    case tok_state::suffix: {
      char ch = peek_next_char();
      if (std::isdigit(static_cast<unsigned char>(ch))) {
        advance();
        accum *= 10;
        accum += to_digit(ch);
        break;
      } else {
        state = tok_state::prefix;
        return create_token<number_token>(accum);
      }
    }
    case tok_state::error:
      fail_error_state();
    case tok_state::end:
      return {};
    }
  }
}
