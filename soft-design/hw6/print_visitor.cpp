#include "print_visitor.h"
#include "grammar.h"

#include <iostream>

void print_visitor::visit(op_token &tok) {
  std::cout << grammar_utils::find_action_by_type(tok.type, [](auto &&ad) {
    return ad.string;
  }) << ' ';
}

void print_visitor::visit(number_token &tok) {
  std::cout << tok.val << ' ';
}
