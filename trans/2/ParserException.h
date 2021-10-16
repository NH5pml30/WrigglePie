#pragma once

#include <format>
#include <sstream>
#include <stdexcept>
#include <string>

class ParserException : public std::runtime_error
{
public:
  const int pos;

  ParserException(const std::string &message, int pos)
      : std::runtime_error(std::format("{}: {}", pos, message)), pos(pos)
  {
  }
};

void print_exception_(std::stringstream &ss, const std::exception &e, int level)
{
  ss << std::string(level, '>') << e.what() << '\n';
  try
  {
    std::rethrow_if_nested(e);
  }
  catch (const std::exception &e)
  {
    print_exception_(ss, e, level + 1);
  }
  catch (...)
  {
  }
}

std::string print_exception(const std::exception &e, int level = 0)
{
  std::stringstream ss;
  print_exception_(ss, e, level);
  return ss.str();
}
