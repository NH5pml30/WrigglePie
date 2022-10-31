#pragma once

#include "token_visitor.h"

class print_visitor : public rpn_token_visitor {
public:
  using rpn_token_visitor::visit;

  void visit(number_token &tok) override;
  void visit(op_token &tok) override;
};
