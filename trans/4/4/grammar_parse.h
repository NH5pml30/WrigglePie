#pragma once

#include "config.h"

#include <string>
#include <vector>
#include <ranges>
#include <unordered_map>

namespace _grammar_parse
{
  struct lexer_record
  {
    std::string name;
    std::string regex;
    bool passed;
    int cached_id{};
  };

  template<typename T>
  std::vector<T> &&append(std::vector<T> &v, T &&t)
  {
    v.push_back(std::move(t));
    return std::move(v);
  }

  using lexer_block = std::vector<lexer_record>;

  struct parser_rule_rhs
  {
    std::vector<std::string> rhs;
    std::string attribute;
  };

  struct parser_record
  {
    std::string nt_name;
    std::string type;
    std::vector<parser_rule_rhs> rule_rhss;
    int cached_id{};
  };

  using parser_block = std::vector<parser_record>;

  inline parser_configuration to_config(std::string preamble, lexer_block l_block,
                                        parser_block p_block)
  {
    static std::regex brace_reg = std::regex("\\\\(\\{)|\\\\(\\})");
    static std::regex slash_reg = std::regex("\\\\/");

    std::unordered_map<std::string, grammar::element> el_map;

    el_map.insert({"EOF", 0});
    el_map.insert({"$start", 1});  // add phantom starting state
    for (auto &record : p_block)
      el_map.insert({record.nt_name, (grammar::nonterminal)el_map.size()});  /// try rewrite old?
    int max_nonterminal = (int)el_map.size();
    for (auto &m : l_block)
      el_map.insert({m.name, m.cached_id = (grammar::terminal)el_map.size()});  /// try rewrite old?
    int max_element = (int)el_map.size();

    std::vector<std::string> types(max_nonterminal - 1);
    types[0] = p_block[0].type;
    std::vector<std::pair<grammar::nonterminal, grammar::rule>> rules = {
      {1, {{el_map[p_block[0].nt_name]}, "{std::move($1)}"}}
    };
    for (auto &record : p_block)
    {
      types[(record.cached_id = el_map[record.nt_name]) - 1] = record.type;  /// try rewrite old?

      for (auto &rule_rhs : record.rule_rhss)
      {
        using namespace std::placeholders;
        std::vector<grammar::element> rhs;
        std::ranges::transform(rule_rhs.rhs, std::back_inserter(rhs),
                               [&el_map](auto &&x) { return el_map[x]; });
        // replace escaped {} and wrap
        std::string res = "{";
        std::regex_replace(std::back_inserter(res), rule_rhs.attribute.begin(),
                           rule_rhs.attribute.end(), brace_reg, "$1$2");
        res.push_back('}');
        rules.emplace_back(record.cached_id, grammar::rule{std::move(rhs), std::move(res)});
      }
    }

    std::vector<std::string> print_data(el_map.size());
    for (auto &&[lhs, rhs] : el_map)
      print_data[rhs] = lhs;

    std::vector<std::pair<std::string, bool>> matchers(max_element - max_nonterminal);
    for (auto &m : l_block)
    {
      // replace escapes and trim the slashes
      std::string res = std::regex_replace(m.regex.data() + 1, slash_reg, "/");
      res.pop_back();
      matchers[m.cached_id - max_nonterminal] = {std::move(res), m.passed};
    }

    return parser_configuration{
        preamble,
        [print_data = std::move(print_data)](grammar::element el) { return print_data[el]; },
        matchers, grammar(std::move(types), max_nonterminal, max_element, std::move(rules))
    };
  }
}  // namespace _grammar_parse
