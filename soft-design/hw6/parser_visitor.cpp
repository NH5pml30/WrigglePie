#include "parser_visitor.h"
#include "grammar.h"

void parser_visitor::drop_opers(token &tok) {
  while (!operation_stack.empty()) {
    auto &top_tok = *operation_stack.back();
    grammar_utils::get_prior p;
    tok.accept(p);
    int tok_prior = p.result;
    top_tok.accept(p);
    int top_tok_prior = p.result;

    if (top_tok_prior < tok_prior)
      break;

    operation_stack.pop_back();
    operand_stack.push_back(&top_tok);
  }
}

[[noreturn]] void parser_visitor::fail(tok_pos pos, const std::string &msg) {
  __super::fail(parser_exception(msg, pos));
}

void parser_visitor::visit(op_token &tok) {
  switch (state) {
  case parser_state::suffix:
    drop_opers(tok);
    operation_stack.push_back(&tok);
    state = parser_state::prefix;
    break;
  case parser_state::error:
    fail_error_state();
  default:
    fail(tok.pos, "Operation is not expected");
  }
}

void parser_visitor::visit(number_token &tok) {
  switch (state) {
  case parser_state::prefix:
    operand_stack.push_back(&tok);
    state = parser_state::suffix;
    break;
  case parser_state::error:
    fail_error_state();
  default:
    fail(tok.pos, "Number is not expected");
  }
}

void parser_visitor::visit(brace_token &tok) {
  switch (state) {
  case parser_state::prefix:
    if (tok.is_closing)
      fail(tok.pos, "Expected number or opening brace, not closing brace");
    operation_stack.push_back(&tok);
    break;
  case parser_state::suffix:
    if (!tok.is_closing)
      fail(tok.pos, "Expected operation or closing brace, not opening brace");
    drop_opers(tok);
    if (operation_stack.empty())
      fail(tok.pos, "Could not find matching opening brace");
    operation_stack.pop_back();
    break;
  case parser_state::error:
    fail_error_state();
  }
}

std::vector<token *> parser_visitor::finish() {
  switch (state) {
  case parser_state::suffix: {
    auto closing_brace = brace_token({}, true);
    drop_opers(closing_brace);
    if (!operation_stack.empty())
      fail(operation_stack.back()->pos, "Could not find matching closing brace");
    auto to_ret = std::move(operand_stack);
    reinit();
    return to_ret;
  }
  case parser_state::error:
    fail_error_state();
  default:
    fail(tok_pos::end_pos(), "Unexpected end-of-expression");
  }
}
