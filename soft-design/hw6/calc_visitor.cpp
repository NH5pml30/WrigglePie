#include <cassert>

#include "calc_visitor.h"
#include "grammar.h"

void calc_visitor::visit(op_token &tok) {
  operand_stack.push(grammar_utils::eval(tok.type, std::bind_front(&calc_visitor::pop_arg, this)));
}

void calc_visitor::visit(number_token &tok) {
  operand_stack.push(tok.val);
}

eval_t calc_visitor::finish() {
  assert(operand_stack.size() == 1);
  auto res = operand_stack.top();
  operand_stack.pop();
  return res;
}
