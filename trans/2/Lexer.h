#pragma once

#include <iostream>
#include <string>
#include <format>

#include "ParserException.h"
#include "Token.h"

class Lexer
{
  std::istream &is;
  int cur_char, cur_pos_;
  Token cur_token_;

  void next_char()
  {
    cur_pos_++;
    try
    {
      cur_char = is.get();
    }
    catch (std::ios_base::failure &)
    {
      std::throw_with_nested(create_exception("IO exception occured"));
    }
  }

  void push_token(Token token)
  {
    next_char();
    cur_token_ = token;
  }

public:
  Lexer(std::istream &is) : is(is), cur_pos_(0)
  {
    is.exceptions(is.badbit);
    next_char();
  }

  ParserException create_exception(const std::string &msg)
  {
    return ParserException(msg, cur_pos_);
  }

  [[noreturn]] void fail(const std::string &msg)
  {
    throw create_exception(msg);
  }

  void ensure_token(Token expected, const std::string &msg)
  {
    if (cur_token_ != expected)
      fail(msg);
  }

  void next_token()
  {
    while (std::isspace(cur_char) && cur_char != '\n')
      next_char();
    switch (cur_char)
    {
    case '(':
      push_token(Token::LPAREN);
      break;
    case ')':
      push_token(Token::RPAREN);
      break;
    case '|':
      push_token(Token::OR);
      break;
    case '&':
      push_token(Token::AND);
      break;
    case '^':
      push_token(Token::XOR);
      break;
    case '!':
      push_token(Token::NOT);
      break;
    case -1:
    case '\n':
      push_token(Token::END);
      break;
    default:
      if (std::isalpha(cur_char))
        push_token(Token::VAR);
      else
        throw create_exception(std::format("Illegal character '{:c}'", cur_char));
    }
  }

  Token cur_token()
  {
    return cur_token_;
  }

  int cur_pos()
  {
    return cur_pos_;
  }
};
