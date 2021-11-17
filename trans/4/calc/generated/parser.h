#pragma once

#include <cstdlib>

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

public:
  LALR_parser()
  {
    _read_table();
  }

  double parse(std::istream &i)
  {
    using _attr_type0 = double;
    using _attr_type1 = double;
    using _attr_type2 = double;
    using _attr_type3 = double;

    _the_lexer.set_input(i);

    using _work_data_type = std::variant<unsigned, std::string, double, double, double, double>;
    std::vector<_work_data_type> _work;
    _work.emplace_back(_work_data_type{std::in_place_index<0>, 0u});

    bool _to_continue = true;
    std::string $0;
    size_t _last_token_len {};

    while (_to_continue)
    {
      unsigned _state = std::get<0>(_work.back());

      std::visit(overloaded{[&](std::monostate) { _the_lexer.fail("Parse error"); },
                            [&](_shift_action _act) {
                              _work.push_back(_work_data_type{std::in_place_index<1>, std::move(_the_lexer.cur_token().str)});
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
                                  $n = _work_data_type{std::in_place_index<3>, _attr_type1{ $1 + $3 }};
                                  _lhs = 2;
                                  break;
                                }
                                case 2:
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
                                  $n = _work_data_type{std::in_place_index<3>, _attr_type1{ $1 - $3 }};
                                  _lhs = 2;
                                  break;
                                }
                                case 3:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<4>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<3>, _attr_type1{ $1 }};
                                  _lhs = 2;
                                  break;
                                }
                                case 4:
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
                                  $n = _work_data_type{std::in_place_index<4>, _attr_type2{ $1 * $3 }};
                                  _lhs = 3;
                                  break;
                                }
                                case 5:
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
                                  $n = _work_data_type{std::in_place_index<4>, _attr_type2{ $1 / $3 }};
                                  _lhs = 3;
                                  break;
                                }
                                case 6:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<5>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<4>, _attr_type2{ $1 }};
                                  _lhs = 3;
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
                                  $n = _work_data_type{std::in_place_index<5>, _attr_type3{ $2 }};
                                  _lhs = 4;
                                  break;
                                }
                                case 8:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<5>, _attr_type3{ std::atof($1.c_str()) }};
                                  _lhs = 4;
                                  break;
                                }
                                case 9:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<5>, _attr_type3{ std::atof($1.c_str()) }};
                                  _lhs = 4;
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
