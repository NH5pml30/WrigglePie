#pragma once
#include"grammar_parse.h"
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
    std::ifstream in("./parser_table.dat", std::ios::binary);

    auto convert_data_to_variant = [](_written_data data) -> _action {
      _action res{};
      if (data.type == 1)
        res = _shift_action{data.data};
      else if (data.type == 2)
        res = _reduce_action{data.data};
      return res;
    };

    _trans_table.resize(1188);
    for (auto &el : _trans_table)
    {
      _written_data data {};
      in.read((char *)&data, sizeof(data));
      el = convert_data_to_variant(data);
    }
  }

  _lexer _the_lexer;

public:
  LALR_parser()
  {
    _read_table();
  }

  parser_configuration parse(std::istream &i)
  {
    _the_lexer.set_input(i);

    using _work_data_type = std::variant<unsigned, std::string, parser_configuration, parser_configuration, std::string, _grammar_parse::lexer_block, _grammar_parse::lexer_block, _grammar_parse::lexer_record, _grammar_parse::parser_block, _grammar_parse::parser_block, _grammar_parse::parser_record, std::vector<_grammar_parse::parser_rule_rhs>, _grammar_parse::parser_rule_rhs, std::vector<std::string>, std::string, std::string>;
    std::vector<_work_data_type> _work = {_work_data_type{std::in_place_index<0>, 0u}};

    bool _to_continue = true;

    while (_to_continue)
    {
      unsigned _state = std::get<0>(_work.back());

      std::visit(overloaded{[&](std::monostate) { _the_lexer.fail("Parse error"); },
                            [&](_shift_action _act) {
                              _work.push_back(_work_data_type{std::in_place_index<1>, std::move(_the_lexer.cur_token().str)});
                              _work.push_back(_work_data_type{std::in_place_index<0>, _act.next_state});
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
                                  $n = _work_data_type{std::in_place_index<2>, parser_configuration{std::move($1)}};
                                  _to_continue = false;
                                  break;
                                }
                                case 1:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<8>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<5>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<4>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<3>, parser_configuration{_grammar_parse::to_config(std::move($1),std::move($2),std::move($3))}};
                                  _lhs = 2;
                                  break;
                                }
                                case 2:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<14>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<4>, std::string{$1}};
                                  _lhs = 3;
                                  break;
                                }
                                case 3:
                                {
                                  _work.pop_back();
                                  auto $4 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $3 = std::get<6>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<5>, _grammar_parse::lexer_block{std::move($3)}};
                                  _lhs = 4;
                                  break;
                                }
                                case 4:
                                {
                                  $n = _work_data_type{std::in_place_index<6>, _grammar_parse::lexer_block{}};
                                  _lhs = 5;
                                  break;
                                }
                                case 5:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<7>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<6>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<6>, _grammar_parse::lexer_block{_grammar_parse::append($1,std::move($2))}};
                                  _lhs = 5;
                                  break;
                                }
                                case 6:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<7>, _grammar_parse::lexer_record{std::move($1),std::move($2),true}};
                                  _lhs = 6;
                                  break;
                                }
                                case 7:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<7>, _grammar_parse::lexer_record{std::move($1),std::move($2),false}};
                                  _lhs = 6;
                                  break;
                                }
                                case 8:
                                {
                                  _work.pop_back();
                                  auto $4 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $3 = std::get<9>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<8>, _grammar_parse::parser_block{std::move($3)}};
                                  _lhs = 7;
                                  break;
                                }
                                case 9:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<10>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<9>, _grammar_parse::parser_block{std::move($1)}};
                                  _lhs = 8;
                                  break;
                                }
                                case 10:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<10>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<9>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<9>, _grammar_parse::parser_block{_grammar_parse::append($1,std::move($2))}};
                                  _lhs = 8;
                                  break;
                                }
                                case 11:
                                {
                                  _work.pop_back();
                                  auto $5 = std::get<11>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $4 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $3 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<10>, _grammar_parse::parser_record{std::move($1),std::move($3),std::move($5)}};
                                  _lhs = 9;
                                  break;
                                }
                                case 12:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<12>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<11>, std::vector<_grammar_parse::parser_rule_rhs>{std::move($1)}};
                                  _lhs = 10;
                                  break;
                                }
                                case 13:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<12>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<11>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<11>, std::vector<_grammar_parse::parser_rule_rhs>{_grammar_parse::append($1,std::move($3))}};
                                  _lhs = 10;
                                  break;
                                }
                                case 14:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<14>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<13>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<12>, _grammar_parse::parser_rule_rhs{std::move($1),std::move($2)}};
                                  _lhs = 11;
                                  break;
                                }
                                case 15:
                                {
                                  $n = _work_data_type{std::in_place_index<13>, std::vector<std::string>{}};
                                  _lhs = 12;
                                  break;
                                }
                                case 16:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<13>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<13>, std::vector<std::string>{_grammar_parse::append($1,std::move($2))}};
                                  _lhs = 12;
                                  break;
                                }
                                case 17:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<14>, std::string{$2}};
                                  _lhs = 13;
                                  break;
                                }
                                case 18:
                                {
                                  $n = _work_data_type{std::in_place_index<15>, std::string{""}};
                                  _lhs = 14;
                                  break;
                                }
                                case 19:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                                case 20:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                                case 21:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                                case 22:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                                case 23:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                                case 24:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                                case 25:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                                case 26:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                                case 27:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                                case 28:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, std::string{$1+$2}};
                                  _lhs = 14;
                                  break;
                                }
                              }

                              unsigned _rollback_state = std::get<0>(_work.back());
                              _work.push_back(std::move($n));

                              if (_to_continue)
                                _work.push_back(std::get<_shift_action>(
                                                   _trans_table[_rollback_state * 27 + _lhs])
                                                   .next_state);
                            }
        }, _trans_table[_state * 27 + _the_lexer.cur_token().token_id]);
    }
    return std::get<2>(_work.back());
  }
};
