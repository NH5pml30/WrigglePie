#pragma once

#include "control_auto.h"

struct parser_generator
{
public:
  std::string preamble;
  std::function<std::string(int)> printer;
  std::vector<std::pair<std::string, bool>> nonterminal_matchers;

  control_automaton automaton;

  parser_generator(parser_configuration conf)
      : preamble(std::move(conf.preamble)),
        printer(std::move(conf.printer)),
        nonterminal_matchers(std::move(conf.nonterminal_matchers)),
        automaton(std::move(conf.G), printer)
  {
  }

  struct out_formatter
  {
    std::ofstream out;
    std::string indent_str;

    out_formatter(const std::string &filename) : out(filename) {
    }

    void increase_indent(unsigned delta) {
      indent_str.resize(indent_str.size() + delta * 2, ' ');
    }

    void decrease_indent(unsigned delta) {
      indent_str.resize(indent_str.size() - delta * 2);
    }

    template<typename T>
    std::ofstream &operator<<(T &&arg)
    {
      out << indent_str << std::forward<T>(arg);
      return out;
    }
  };

  void generate_lexer_file(const std::string &filename) const
  {
    out_formatter fmt(filename);

    fmt << R"__delim(#pragma once

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
  int cache_level = 0;
  std::stringstream cache, hidden_cache;
  std::string commit_cache_;
  token cur_token_;
  std::vector<std::pair<std::regex, bool>> matchers = {
)__delim";
    fmt.increase_indent(2);
    fmt.out << std::boolalpha;
    for (auto &&[str, passed] : nonterminal_matchers)
      fmt << "{std::regex(R\"__del(" << str << ")__del\"), " << passed << "},\n";
    fmt.decrease_indent(2);
    fmt.out << std::noboolalpha;
    fmt << R"__delim(  };

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
      cur_line.push_back('\n');
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
    {
      if (cache_level > 0)
        cache << cur_token_.str << hidden_cache.str();
      cur_token_ = token{)__delim" << automaton.G.max_nonterminal << R"__delim( + id, cur_line.substr(cur_line_pos, length)};
    }
    else
      hidden_cache << cur_line.substr(cur_line_pos, length);
    cur_line_pos += length;
  }

public:
  _lexer()
  {
  }

  void push_caching() {
    if (cache_level++ == 0)
      cache << hidden_cache.str();
  }

  bool pop_caching() {
    if (--cache_level == 0)
    {
      commit_cache_ = cache.str();
      commit_cache_.resize(commit_cache_.size() - hidden_cache.str().size());
      cache.str("");
      return true;
    }
    return false;
  }

  std::string commit_cache() {
    return commit_cache_;
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
    bool passed_through = false;
    hidden_cache.str("");
    do
    {
      if (cur_line_pos == cur_line.size())
        if (!next_line())
        {
          cur_token_ = token{0, ""};
          return;
        }

      std::smatch m;
      int id = 0;
      for (auto &&[matcher, passed] : matchers)
      {
        if (std::regex_search(cur_line.cbegin() + cur_line_pos, cur_line.cend(), m, matcher, std::regex_constants::match_continuous))
        {
          push_token(id, passed, (int)m.length());
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
)__delim";
  }

  void generate_parser_file(unsigned n_states, const std::string &table_filename,
                            const std::string &lexer_filename, const std::string &parser_filename) const
  {
    out_formatter fmt(parser_filename);
    auto &G = automaton.G;

    std::stringstream type_s;
    bool is_first = true;
    for (auto &el : G.attr_types)
    {
      if (!is_first)
        type_s << ", ";
      type_s << el;
      is_first = false;
    }

    auto nt2index = [](const std::pair<grammar::nonterminal, grammar::rule> &r) -> int { return r.first - 1; };
    auto rule_lhs2type = [&](unsigned rule_id) -> const std::string & {
      return G.attr_types[nt2index(G[rule_id])];
    };
    auto nt2work = [](const std::pair<grammar::nonterminal, grammar::rule> &r) -> int { return r.first - 1 + 2; };

    std::string attr_type = type_s.str();
    std::string start_attr_type = rule_lhs2type(0);

    fmt << "#pragma once\n";
    fmt << preamble;
    fmt << R"__delim(
#include <fstream>
#include <variant>
#include <vector>

#include ")__delim" << lexer_filename << R"__delim("

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
    std::ifstream in(")__delim" << table_filename << R"__delim(", std::ios::binary);

    if (!in.is_open())
      throw parser_exception("Cannot open the parser table (expecting a valid file ')__delim"
        << table_filename << R"__delim(')", 0, 0);

    auto convert_data_to_variant = [](_written_data data) -> _action {
      _action res{};
      if (data.type == 1)
        res = _shift_action{data.data};
      else if (data.type == 2)
        res = _reduce_action{data.data};
      return res;
    };

    _trans_table.resize()__delim" << n_states * G.max_element << R"__delim();
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

  )__delim"
        << start_attr_type << R"__delim( parse(std::istream &i)
  {
)__delim";
    fmt.increase_indent(2);
    for (unsigned i = 0; i < (unsigned)G.attr_types.size(); i++)
      fmt << "using _attr_type" << i << " = " << G.attr_types[i] << ";\n";
    fmt.decrease_indent(2);
    fmt << R"__delim(
    _the_lexer.set_input(i);

    using _work_data_type = std::variant<unsigned, std::string, )__delim" << attr_type << R"__delim(>;
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
)__delim";
    unsigned rule_id = 0;
    fmt.increase_indent(15);
    for (auto &r : G)
    {
      // write specific action for each rule
      fmt.increase_indent(1);
      fmt << "case " << rule_id << ":\n";
      fmt << "{\n";
      fmt.increase_indent(1);
      // handle caching mode actions
      switch (G[rule_id].second.cache_mod)
      {
      case grammar::cache_action::PUSH:
        fmt << "_the_lexer.push_caching();\n";
        break;
      case grammar::cache_action::POP:
        fmt << "if (_the_lexer.pop_caching())\n";
        fmt << "{\n";
        fmt.increase_indent(1);
        fmt << "$0 = _the_lexer.commit_cache();\n";
        // do not count the preread token
        fmt << "$0.resize($0.size() - _last_token_len);\n";
        fmt.decrease_indent(1);
        fmt << "}\n";
        break;
      }

      // pop arguments from the stack and compute attribute
      for (unsigned i = (unsigned)G[rule_id].second.rhs.size(); i > 0; i--)
      {
        fmt << "_work.pop_back();\n";
        unsigned index = 0;
        if (grammar::element el = G[rule_id].second.rhs[i - 1];
            G.is_nonterminal(el))
          index = el + 1;
        else
          index = 1;
        fmt << "auto $" << i << " = std::get<" << index << ">(std::move(_work.back()));\n";
        fmt << "_work.pop_back();\n";
      }
      fmt << "$n = _work_data_type{std::in_place_index<" << nt2work(G[rule_id]) << ">, _attr_type"
          << nt2index(G[rule_id]) << G[rule_id].second.atribute << "};\n";
      if (rule_id == 0)
        fmt << "_to_continue = false;\n";
      else
        fmt << "_lhs = " << G[rule_id].first << ";\n";
      fmt << "break;\n";
      fmt.decrease_indent(1);
      fmt << "}\n";
      fmt.decrease_indent(1);
      rule_id++;
    }
    fmt << "}\n";

    fmt.decrease_indent(15);
    fmt << R"__delim(
                              unsigned _rollback_state = std::get<0>(_work.back());
                              _work.push_back(std::move($n));

                              if (_to_continue)
                                _work.push_back(std::get<_shift_action>(
                                                   _trans_table[_rollback_state * )__delim" << G.max_element << R"__delim( + _lhs])
                                                   .next_state);
                            }
        }, _trans_table[_state * )__delim" << G.max_element << R"__delim( + _the_lexer.cur_token().token_id]);
    }
    return std::get<)__delim" << nt2work(G[0]) << R"__delim(>(std::move(_work.back()));
  }
};
)__delim";
  }

  void generate_files(const std::string &dir, const std::string &table_filename,
                      const std::string &lexer_filename,
                      const std::string &parser_filename) const
  {
    automaton.print_table(printer);
    unsigned n_states = automaton.save_table(dir + "/" + table_filename);
    generate_lexer_file(dir + "/" + lexer_filename);
    generate_parser_file(n_states, dir + "/" + table_filename, lexer_filename,
                         dir + "/" + parser_filename);
  }
};
