#pragma once

#include "grammar.h"

#include <stack>

class calc_visitor : public rpn_token_visitor {
public:
  using rpn_token_visitor::visit;

  void visit(number_token &tok) override;
  void visit(op_token &tok) override;

  eval_t finish();

private:
  eval_t pop_arg() {
    auto res = operand_stack.top();
    operand_stack.pop();
    return res;
  }

  std::stack<eval_t> operand_stack;
};
