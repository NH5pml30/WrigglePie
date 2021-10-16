#pragma once

#include <memory>

#include "Lexer.h"
#include "Tree.h"

class Parser
{
  Lexer lexer;

  std::unique_ptr<Tree> E()
  {
    switch (lexer.cur_token())
    {
    case Token::VAR:
    case Token::LPAREN:
    case Token::NOT:
      return NodeBuilder("E").add(T()).add(Ep()).finish();
    default:
      lexer.fail("Variable, '(' or '!' expected");
    }
  }

  std::unique_ptr<Tree> Ep()
  {
    switch (lexer.cur_token())
    {
    case Token::OR:
      lexer.next_token();
      return NodeBuilder("E'").add("|").add(T()).add(Ep()).finish();
    case Token::END:
    case Token::RPAREN:
      return node("E'");
    default:
      lexer.fail("'|', end of input or ')' expected");
    }
  }

  std::unique_ptr<Tree> T()
  {
    switch (lexer.cur_token())
    {
    case Token::VAR:
    case Token::LPAREN:
    case Token::NOT:
      return NodeBuilder("T").add(F()).add(Tp()).finish();
    default:
      lexer.fail("Variable, '(' or '!' expected");
    }
  }

  std::unique_ptr<Tree> Tp()
  {
    switch (lexer.cur_token())
    {
    case Token::XOR:
      lexer.next_token();
      return NodeBuilder("T'").add("^").add(F()).add(Tp()).finish();
    case Token::END:
    case Token::RPAREN:
    case Token::OR:
      return node("T'");
    default:
      lexer.fail("'|', '^', end of input or ')' expected");
    }
  }

  std::unique_ptr<Tree> F()
  {
    switch (lexer.cur_token())
    {
    case Token::VAR:
    case Token::LPAREN:
    case Token::NOT:
      return NodeBuilder("F").add(G()).add(Fp()).finish();
    default:
      lexer.fail("Variable, '(' or '!' expected");
    }
  }

  std::unique_ptr<Tree> Fp()
  {
    switch (lexer.cur_token())
    {
    case Token::AND:
      lexer.next_token();
      return NodeBuilder("F'").add("&").add(G()).add(Fp()).finish();
    case Token::END:
    case Token::RPAREN:
    case Token::OR:
    case Token::XOR:
      return node("F'");
    default:
      lexer.fail("'&', '|', '^', end of input or ')' expected");
    }
  }

  std::unique_ptr<Tree> G()
  {
    auto builder = NodeBuilder("G");
    switch (lexer.cur_token())
    {
    case Token::VAR:
      lexer.next_token();
      return std::move(builder).add("n").finish();
    case Token::LPAREN:
    {
      int start = lexer.cur_pos();
      lexer.next_token();
      builder.add("(").add(E());
      lexer.ensure_token(Token::RPAREN,
                         std::format("Matching ')' for '(' at position {} not found", start));
      lexer.next_token();
      return std::move(builder).add(")").finish();
    }
    case Token::NOT:
      lexer.next_token();
      return std::move(builder).add("!").add(G()).finish();
    default:
      lexer.fail("Variable, '(' or '!' expected");
    }
  }

  Parser(std::istream &is) : lexer(is)
  {
    lexer.next_token();
  }

public:
  static std::unique_ptr<Tree> parse(std::istream &is)
  {
    Parser p = Parser(is);
    auto res = p.E();
    p.lexer.ensure_token(Token::END, "End of input expected");
    return res;
  }
};
