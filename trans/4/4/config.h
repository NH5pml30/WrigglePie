#pragma once

#include <regex>
#include <fstream>
#include <sstream>

#include "grammar.h"

struct parser_configuration
{
  std::string preamble;
  std::function<std::string(int)> printer;
  std::vector<std::pair<std::string, bool>> nonterminal_matchers;
  grammar G;
};
