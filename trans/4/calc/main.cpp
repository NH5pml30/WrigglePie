#include "generated/parser.h"

#include <fstream>
#include <iostream>

int main() {
  try
  {
    std::cout << LALR_parser().parse(std::cin);
  }
  catch (std::exception &e)
  {
    std::cout << print_exception(e);
  }
  return 0;
}
