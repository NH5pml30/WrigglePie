#pragma once

#include <format>
#include <iostream>
#include <regex>
#include <string>
#include <sstream>

class parser_exception : public std::runtime_error
{
public:
  const int line, col;

  parser_exception(const std::string &message, int line, int col)
      : std::runtime_error(std::format("{}:{}: {}", line, col, message)), line(line), col(col)
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

class _lexer
{
  std::istream *is {};

  std::string cur_line;
  int cur_line_idx = 0, cur_line_pos = 0;
public:
  struct token
  {
    int token_id;
    std::string str;
  };

private:
  token cur_token_;
  std::vector<std::pair<std::regex, bool>> matchers = {
    {std::regex(R"__del(^\s+)__del"), false},
    {std::regex(R"__del(^\d+\.\d+)__del"), true},
    {std::regex(R"__del(^\d+)__del"), true},
    {std::regex(R"__del(^\+)__del"), true},
    {std::regex(R"__del(^\-)__del"), true},
    {std::regex(R"__del(^\*)__del"), true},
    {std::regex(R"__del(^/)__del"), true},
    {std::regex(R"__del(^\()__del"), true},
    {std::regex(R"__del(^\))__del"), true},
  };

  bool next_line()
  {
    try
    {
      bool res = true;
      do {
        res &= !!std::getline(*is, cur_line);
        cur_line_pos = 0;
        cur_line_idx++;
      } while (res && cur_line.empty());
      return res;
    }
    catch (std::ios_base::failure &)
    {
      std::throw_with_nested(create_exception("IO exception occured"));
    }
  }

  void push_token(int id, bool passed, int length)
  {
    if (passed)
      cur_token_ = token{5 + id, cur_line.substr(cur_line_pos, length)};
    cur_line_pos += length;
  }

public:
  _lexer()
  {
  }

  void set_input(std::istream &is)
  {
    this->is = &is;
    cur_line_idx = cur_line_pos = 0;
    is.exceptions(is.badbit);
    next_token();
  }

  parser_exception create_exception(const std::string &msg)
  {
    return parser_exception(msg, cur_line_idx, cur_line_pos);
  }

  [[noreturn]] void fail(const std::string &msg)
  {
    throw create_exception(msg);
  }

  void next_token()
  {
    if (cur_line_pos == cur_line.size())
      if (!next_line())
      {
        cur_token_ = token{0, ""};
        return;
      }

    bool passed_through = false;
    do
    {
      std::smatch m;
      int id = 0;
      for (auto &&[matcher, passed] : matchers)
      {
        if (std::regex_search(cur_line.cbegin() + cur_line_pos, cur_line.cend(), m, matcher))
        {
          push_token(id, passed, m.length());
          passed_through |= passed;
          break;
        }
        id++;
      }
      if (m.empty())
        throw create_exception("No tokens matched");
    } while(!passed_through);
  }

  const token &cur_token()
  {
    return cur_token_;
  }
};