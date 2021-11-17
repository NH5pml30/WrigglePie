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

  using parse_rule_content_element = std::variant<std::string, std::vector<std::string>>;

  inline parse_rule_content_element content_el_from_terminal(std::string data) {
    return parse_rule_content_element{std::in_place_index<0>, std::move(data)};
  }

  inline parse_rule_content_element content_el_from_tilde(std::vector<std::string> data) {
    return parse_rule_content_element{std::in_place_index<1>, std::move(data)};
  }

  struct parser_rule_rhs
  {
    std::vector<parse_rule_content_element> rhs;
    grammar::cache_action cache_mod;
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
      {1, {{el_map[p_block[0].nt_name]}, grammar::cache_action::NONE, "{std::move($1)}"}}
    };
    for (auto &record : p_block)
    {
      types[(record.cached_id = el_map[record.nt_name]) - 1] = record.type;  /// try rewrite old?

      for (auto &rule_rhs : record.rule_rhss)
      {
        // replace escaped {} and wrap
        std::string attr = "{";
        std::regex_replace(std::back_inserter(attr), rule_rhs.attribute.begin(),
                           rule_rhs.attribute.end(), brace_reg, "$1$2");
        attr.push_back('}');

        // process inverted terminals (~)
        std::vector<std::variant<grammar::element, std::vector<grammar::terminal>>> preproc;
        for (auto &content_el : rule_rhs.rhs)
        {
          if (content_el.index() == 0)
            preproc.emplace_back(std::in_place_index<0>, el_map[std::get<0>(content_el)]);
          else
          {
            std::vector<grammar::terminal> ts;
            std::ranges::transform(std::get<1>(content_el), std::back_inserter(ts),
                                   [&el_map](auto &&x) { return el_map[x]; }); /// assert terminal?
            preproc.emplace_back(std::in_place_index<1>, std::move(ts));
          }
        }

        // for each inverted set generate all the possible rules (might be a lot)
        auto gen = [&, res = std::vector<grammar::element>()](auto &gen) mutable {
          if (res.size() == preproc.size())
          {
            rules.emplace_back(record.cached_id, grammar::rule{res, rule_rhs.cache_mod, attr});
            return;
          }

          size_t i = res.size();
          if (preproc[i].index() == 0)
          {
            res.push_back(std::get<0>(preproc[i]));
            return gen(gen);
          }

          const auto &not_set = std::get<1>(preproc[i]);
          for (grammar::terminal t = max_nonterminal; t < max_element; t++)
            if (std::ranges::find(not_set, t) == not_set.end())
            {
              res.push_back(t);
              gen(gen);
              res.pop_back();
            }
        };
        gen(gen);
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
