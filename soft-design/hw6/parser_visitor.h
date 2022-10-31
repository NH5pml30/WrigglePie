#pragma once

#include "parser_exception.h"
#include "token_visitor.h"

#include <vector>

class parser_visitor : public token_visitor, public errorable_state_machine_base<parser_visitor> {
  friend class errorable_state_machine_base<parser_visitor>;
public:
  parser_visitor() {
    reinit();
  }

  void visit(number_token &tok) override;
  void visit(brace_token &tok) override;
  void visit(op_token &tok) override;

  std::vector<token *> finish();

private:
  enum class parser_state { prefix, suffix, error } state{};
  using state_t = parser_state;

  void reinit() {
    state = parser_state::prefix;
    operand_stack.clear();
    operation_stack.clear();
  }

  void drop_opers(token &tok);

  [[noreturn]] void fail(tok_pos pos, const std::string &msg);

  std::vector<token *> operand_stack, operation_stack;
};
