#pragma once

#include "grammar_parse.h"

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

    _trans_table.resize(2304);
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
    using _attr_type0 = parser_configuration;
    using _attr_type1 = parser_configuration;
    using _attr_type2 = std::string;
    using _attr_type3 = _grammar_parse::lexer_block;
    using _attr_type4 = _grammar_parse::lexer_block;
    using _attr_type5 = _grammar_parse::lexer_record;
    using _attr_type6 = _grammar_parse::parser_block;
    using _attr_type7 = _grammar_parse::parser_block;
    using _attr_type8 = _grammar_parse::parser_record;
    using _attr_type9 = std::vector<_grammar_parse::parser_rule_rhs>;
    using _attr_type10 = _grammar_parse::parser_rule_rhs;
    using _attr_type11 = std::vector<_grammar_parse::parse_rule_content_element>;
    using _attr_type12 = std::vector<std::string>;
    using _attr_type13 = std::vector<std::string>;
    using _attr_type14 = int;
    using _attr_type15 = int;
    using _attr_type16 = std::string;
    using _attr_type17 = int;

    _the_lexer.set_input(i);

    using _work_data_type = std::variant<unsigned, std::string, parser_configuration, parser_configuration, std::string, _grammar_parse::lexer_block, _grammar_parse::lexer_block, _grammar_parse::lexer_record, _grammar_parse::parser_block, _grammar_parse::parser_block, _grammar_parse::parser_record, std::vector<_grammar_parse::parser_rule_rhs>, _grammar_parse::parser_rule_rhs, std::vector<_grammar_parse::parse_rule_content_element>, std::vector<std::string>, std::vector<std::string>, int, int, std::string, int>;
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
                                  auto $3 = std::get<8>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<5>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<4>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<3>, _attr_type1{ _grammar_parse::to_config(std::move($1), std::move($2), std::move($3)) }};
                                  _lhs = 2;
                                  break;
                                }
                                case 2:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<18>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<4>, _attr_type2{ $1 }};
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
                                  $n = _work_data_type{std::in_place_index<5>, _attr_type3{ std::move($3) }};
                                  _lhs = 4;
                                  break;
                                }
                                case 4:
                                {
                                  $n = _work_data_type{std::in_place_index<6>, _attr_type4{}};
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
                                  $n = _work_data_type{std::in_place_index<6>, _attr_type4{ _grammar_parse::append($1, std::move($2)) }};
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
                                  $n = _work_data_type{std::in_place_index<7>, _attr_type5{ std::move($1), std::move($2), true }};
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
                                  $n = _work_data_type{std::in_place_index<7>, _attr_type5{ std::move($1), std::move($2), false }};
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
                                  $n = _work_data_type{std::in_place_index<8>, _attr_type6{ std::move($3) }};
                                  _lhs = 7;
                                  break;
                                }
                                case 9:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<10>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<9>, _attr_type7{ std::move($1) }};
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
                                  $n = _work_data_type{std::in_place_index<9>, _attr_type7{ _grammar_parse::append($1, std::move($2)) }};
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
                                  $n = _work_data_type{std::in_place_index<10>, _attr_type8{ std::move($1), std::move($3), std::move($5) }};
                                  _lhs = 9;
                                  break;
                                }
                                case 12:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<12>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<11>, _attr_type9{ std::move($1) }};
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
                                  $n = _work_data_type{std::in_place_index<11>, _attr_type9{ _grammar_parse::append($1, std::move($3)) }};
                                  _lhs = 10;
                                  break;
                                }
                                case 14:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<18>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<13>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<12>, _attr_type10{ std::move($1), grammar::cache_action::NONE, std::move($2) }};
                                  _lhs = 11;
                                  break;
                                }
                                case 15:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<18>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<13>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<12>, _attr_type10{ std::move($1), grammar::cache_action::PUSH, std::move($3) }};
                                  _lhs = 11;
                                  break;
                                }
                                case 16:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<18>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<13>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<12>, _attr_type10{ std::move($1), grammar::cache_action::POP, std::move($3) }};
                                  _lhs = 11;
                                  break;
                                }
                                case 17:
                                {
                                  $n = _work_data_type{std::in_place_index<13>, _attr_type11{}};
                                  _lhs = 12;
                                  break;
                                }
                                case 18:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<13>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<13>, _attr_type11{ _grammar_parse::append($1, _grammar_parse::content_el_from_terminal(std::move($2))) }};
                                  _lhs = 12;
                                  break;
                                }
                                case 19:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<14>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<13>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<13>, _attr_type11{ _grammar_parse::append($1, _grammar_parse::content_el_from_tilde(std::move($2))) }};
                                  _lhs = 12;
                                  break;
                                }
                                case 20:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<14>, _attr_type12{ std::move($2) }};
                                  _lhs = 13;
                                  break;
                                }
                                case 21:
                                {
                                  _work.pop_back();
                                  auto $4 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $3 = std::get<15>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<14>, _attr_type12{ std::move($3) }};
                                  _lhs = 13;
                                  break;
                                }
                                case 22:
                                {
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<15>, _attr_type13{ std::move($1) }};
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
                                  $n = _work_data_type{std::in_place_index<15>, _attr_type13{ _grammar_parse::append($1, std::move($2)) }};
                                  _lhs = 14;
                                  break;
                                }
                                case 24:
                                {
                                  _the_lexer.push_caching();
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<16>, _attr_type14{ 0 }};
                                  _lhs = 15;
                                  break;
                                }
                                case 25:
                                {
                                  if (_the_lexer.pop_caching())
{
                                    $0 = _the_lexer.commit_cache();
                                    $0.resize($0.size() - _last_token_len);
                                  }
                                  _work.pop_back();
                                  auto $1 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<17>, _attr_type15{ 0 }};
                                  _lhs = 16;
                                  break;
                                }
                                case 26:
                                {
                                  _work.pop_back();
                                  auto $3 = std::get<17>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $2 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<16>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<18>, _attr_type16{ std::move($0) }};
                                  _lhs = 17;
                                  break;
                                }
                                case 27:
                                {
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 28:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 29:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 30:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 31:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 32:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 33:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 34:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 35:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 36:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 37:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 38:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 39:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 40:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 41:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 42:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<1>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                                case 43:
                                {
                                  _work.pop_back();
                                  auto $2 = std::get<18>(std::move(_work.back()));
                                  _work.pop_back();
                                  _work.pop_back();
                                  auto $1 = std::get<19>(std::move(_work.back()));
                                  _work.pop_back();
                                  $n = _work_data_type{std::in_place_index<19>, _attr_type17{ 0 }};
                                  _lhs = 18;
                                  break;
                                }
                              }

                              unsigned _rollback_state = std::get<0>(_work.back());
                              _work.push_back(std::move($n));

                              if (_to_continue)
                                _work.push_back(std::get<_shift_action>(
                                                   _trans_table[_rollback_state * 36 + _lhs])
                                                   .next_state);
                            }
        }, _trans_table[_state * 36 + _the_lexer.cur_token().token_id]);
    }
    return std::get<2>(std::move(_work.back()));
  }
};
