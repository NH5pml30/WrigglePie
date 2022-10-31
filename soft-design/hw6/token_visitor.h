#pragma once

#include <stdexcept>

#include "tokens.h"

class token_visitor {
public:
  virtual void visit(number_token &tok) = 0;
  virtual void visit(brace_token &tok) = 0;
  virtual void visit(op_token &tok) = 0;
};

class rpn_token_visitor : public token_visitor {
public:
  void visit(brace_token &tok) override {
    throw std::invalid_argument("There cannot be a brace in reverse polish notation");
  }
};

class gen_op_token_visitor : public token_visitor {
public:
  void visit(number_token &tok) override {
    throw std::invalid_argument("Number token is not an operation");
  }
};
