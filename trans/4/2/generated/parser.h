#pragma once

#include "../Tree.h"

#include <fstream>
#include <variant>
#include <vector>

#include "lexer.h"

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

class LALR_parser
{
private:
  struct _shift_action
  {
    unsigned next_state;
  };

  struct _reduce_action
  {
    unsigned rule_id;
  };

  using _action = std::variant<std::monostate, _shift_action, _reduce_action>;
  std::vector<_action> _trans_table;

  struct _written_data
  {
    uint32_t type : 2;
    uint32_t data : 30;
  };

  void _read_table() {
    std::ifstream in("generated/parser_table.dat", std::ios::binary);

    if (!in.is_open())
      throw parser_exception("Cannot open the parser table (expecting a valid file 'generated/parser_table.dat')", 0, 0);

    auto convert_data_to_variant = [](_written_data data) -> _action {
      _action res{};
      if (data.type == 1)
        res = _shift_action{data.data};
      else if (data.type == 2)
        res = _reduce_action{data.data};
      return res;
    };

    _trans_table.resize(238);
    for (auto &el : _trans_table)
    {
      _written_data data {};
      in.read((char *)&data, sizeof(data));
      el = convert_data_to_variant(data);
    }
  }

  _lexer _the_lexer;
  std::vector<std::string> _symbols = {
    "ws",
    "n",
    "or",
    "xor",
    "and",
    "not",
    "lp",
    "rp",
  };

  std::string _get_symbol(int c) const
  {
    return c == 0 ? "<EOF>" : _symbols[c - 6];
  }

  std::string _get_error_msg(int c, unsigned state) const
  {
    std::stringstream str;
    str << "Parse error: unexpected token: " << _get_symbol(c) << ", expected one of the following: ";
    bool is_first = true;
    for (int el = 6; el < 14; el++)
      if (el != c && _trans_table[state * 14 + el].index() > 0)
        str << (is_first ? (is_first = false, "") : ", ") << _get_symbol(el);
    if (0 != c && _trans_table[state * 14].index() > 0)
      str << (is_first ? (is_first = false, "") : ", ") << _get_symbol(0);
    return str.str();
  }

public:
  LALR_parser()
  {
    _read_table();
  }

  std::unique_ptr<Tree> parse(std::istream &i)
  {
    using _attr_type0 = std::unique_ptr<Tree>;
    using _attr_type1 = std::unique_ptr<Tree>;
    using _attr_type2 = std::unique_ptr<Tree>;
    using _attr_type3 = std::unique_ptr<Tree>;
    using _attr_type4 = std::unique_ptr<Tree>;

    _the_lexer.set_input(i);

    using _work_data_type = std::variant<unsigned, std::string, std::unique_ptr<Tree>, std::unique_ptr<Tree>, std::unique_ptr<Tree>, std::unique_ptr<Tree>, std::unique_ptr<Tree>>;
    std::vector<_work_data_type> _work;
    _work.emplace_back(_work_data_type{std::in_place_index<0>, 0u});

    bool _to_continue = true;
    std::string $0;
    size_t _last_token_len {};

    while (_to_continue)
    {
      unsigned _state = std::get<0>(_work.back());

      std::visit(overloaded{[&](std::monostate) { _the_lexer.fail(_get_error_msg(_the_lexer.cur_token().token_id, _state)); },
                            [&](_shift_action _act) {
                              _work.push_back(_work_data_type{std::in_place_index<1>, _the_lexer.cur_token().str});
                              _work.push_back(_work_data_type{std::in_place_index<0>, _act.next_state});
                              _last_token_len = _the_lexer.cur_token().str.size();
                              _the_lexer.next_token();
                            },
                            [&](_reduce_action _act) {
                              _work_data_type $n;
                              unsigned _lhs {};
                              switch (_act.rule_id)
                              {
                                case 0:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<3>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<2>, _attr_type0{std::move($1)}};
                                  _to_continue = false;
                                  break;
                                }
                                case 1:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<4>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<3>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<3>, _attr_type1{ ternary_op("E", std::move($1), node("|"), std::move($3)) }};
                                  _lhs = 2;
                                  break;
                                }
                                case 2:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<4>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<3>, _attr_type1{ unary_op("E", std::move($1)) }};
                                  _lhs = 2;
                                  break;
                                }
                                case 3:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<5>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<4>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<4>, _attr_type2{ ternary_op("T", std::move($1), node("^"), std::move($3)) }};
                                  _lhs = 3;
                                  break;
                                }
                                case 4:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<5>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<4>, _attr_type2{ unary_op("T", std::move($1)) }};
                                  _lhs = 3;
                                  break;
                                }
                                case 5:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<6>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<5>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<5>, _attr_type3{ ternary_op("F", std::move($1), node("&"), std::move($3)) }};
                                  _lhs = 4;
                                  break;
                                }
                                case 6:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<6>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<5>, _attr_type3{ unary_op("F", std::move($1)) }};
                                  _lhs = 4;
                                  break;
                                }
                                case 7:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<3>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<6>, _attr_type4{ ternary_op("G", node("("), std::move($2), node(")")) }};
                                  _lhs = 5;
                                  break;
                                }
                                case 8:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<6>, _attr_type4{ node("n") }};
                                  _lhs = 5;
                                  break;
                                }
                                case 9:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<6>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<6>, _attr_type4{ binary_op("G", node("!"), std::move($2)) }};
                                  _lhs = 5;
                                  break;
                                }
                              }

                              unsigned _rollback_state = std::get<0>(_work.back());
                              _work.push_back(std::move($n));

                              if (_to_continue)
                                _work.push_back(std::get<_shift_action>(
                                                   _trans_table[_rollback_state * 14 + _lhs])
                                                   .next_state);
                            }
        }, _trans_table[_state * 14 + _the_lexer.cur_token().token_id]);
    }
    return std::get<2>(std::move(_work.back()));
  }
};
