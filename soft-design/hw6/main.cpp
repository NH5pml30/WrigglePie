#include <memory>
#include <iostream>
#include <sstream>

#include "tokenizer.h"
#include "token_visitor.h"
#include "parser_visitor.h"
#include "calc_visitor.h"
#include "print_visitor.h"

int main() {
  auto &ss = std::cin;

  std::vector<std::unique_ptr<token>> tokens;
  std::vector<token *> rpn;

  {
    tokenizer tox(ss);
    parser_visitor pv;
    std::unique_ptr<token> a;

    while (true) {
      try {
        a = tox.next_token();
      } catch (std::exception &e) {
        std::cerr << "No tokenize: " << print_exception(e) << std::endl;
        return 1;
      }

      try {
        if (a) {
          tokens.push_back(std::move(a));
          tokens.back()->accept(pv);
        } else {
          rpn = pv.finish();
          break;
        }
      } catch (std::exception &e) {
        std::cerr << "No parse: " << print_exception(e) << std::endl;
        return 1;
      }
    }
  }

  {
    print_visitor p;
    for (auto tok : rpn)
      tok->accept(p);
    std::cout << '\n';
  }

  {
    calc_visitor c;
    for (auto tok : rpn)
      tok->accept(c);
    std::cout << "= " << c.finish() << std::endl;
  }

  return 0;
}
