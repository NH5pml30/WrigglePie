#if 0

#include "control_auto.h"

int main(int argc, char *argv[]) {

  enum ELEMENTS
  {
    EoF,
    FILE, PREAMBLE_BLOCK,
    LEXER_RECORD, LEXER_RECORDS, LEXER_BLOCK,
    PARSER_RULE_CONTENT, PARSER_RULE_RHS, PARSER_RULE_RHSS, PARSER_RECORD, PARSER_RECORDS, PARSER_BLOCK,
    CURLY_BLOCK_CONTENT, CURLY_BLOCK,
    WS, ID, OPEN_CURLY, CLOSE_CURLY, LEXER_BEGIN, PARSER_BEGIN, REGEX, OR, DOUBLE_COLON, COLON, MINUS, ANY,
    ALL
  };

  std::vector<std::string> prints = {"$",
                                     "file",
                                     "preamble",
                                     "lexer_record",
                                     "lexer_records",
                                     "lexer_block",
                                     "parser_rule_content",
                                     "parser_rule_rhs",
                                     "parser_rule_rhss",
                                     "parser_record",
                                     "parser_records",
                                     "parser_block",
                                     "curly_block_content",
                                     "curly_block",
                                     "_",
                                     "id",
                                     "{",
                                     "}",
                                     "%lexer",
                                     "%parser",
                                     "regex",
                                     "|",
                                     "::",
                                     ":",
                                     "--",
                                     "."};

  std::vector<std::string> types = {
      "parser_configuration",
      "std::string",
      "_grammar_parse::lexer_record",
      "_grammar_parse::lexer_block",
      "_grammar_parse::lexer_block",
      "std::vector<std::string>",
      "_grammar_parse::parser_rule_rhs",
      "std::vector<_grammar_parse::parser_rule_rhs>",
      "_grammar_parse::parser_record",
      "_grammar_parse::parser_block",
      "_grammar_parse::parser_block",
      "std::string",
      "std::string",
  };

  std::vector<std::pair<std::string, bool>> matchers = {{"\\s+", false},
                                                        {"[a-zA-Z_][a-zA-Z_0-9:<>]*", true},
                                                        {"\\{", true},
                                                        {"\\}", true},
                                                        {"%lexer", true},
                                                        {"%parser", true},
                                                        {"/(\\\\/|[^/])*/", true},
                                                        {"\\|", true},
                                                        {"::", true},
                                                        {":", true},
                                                        {"\\-\\-", true},
                                                        {".", true}};

  std::string preamble = "#include \"grammar_parse.h\"\n";

  std::vector<std::pair<grammar::nonterminal, grammar::rule>> rules = {
      {FILE, {{PREAMBLE_BLOCK, LEXER_BLOCK, PARSER_BLOCK}, R"__delim({_grammar_parse::to_config(std::move($1), std::move($2), std::move($3))})__delim"}},
      {PREAMBLE_BLOCK, {{CURLY_BLOCK}, "{$1}"}},
      {LEXER_BLOCK, {{LEXER_BEGIN, OPEN_CURLY, LEXER_RECORDS, CLOSE_CURLY}, "{std::move($3)}"}},
      {LEXER_RECORDS, {{}, "{}"}},
      {LEXER_RECORDS,
       {{LEXER_RECORDS, LEXER_RECORD}, "{_grammar_parse::append($1, std::move($2))}"}},
      {LEXER_RECORD, {{ID, REGEX}, "{std::move($1), std::move($2), false}"}},
      {LEXER_RECORD, {{ID, REGEX, MINUS}, "{std::move($1), std::move($2), true}"}},
      {PARSER_BLOCK, {{PARSER_BEGIN, OPEN_CURLY, PARSER_RECORDS, CLOSE_CURLY}, "{std::move($3)}"}},
      {PARSER_RECORDS, {{}, "{}"}},
      {PARSER_RECORDS,
       {{PARSER_RECORDS, PARSER_RECORD}, "{_grammar_parse::append($1, std::move($2))}"}},
      {PARSER_RECORD,
       {{ID, DOUBLE_COLON, ID, COLON, PARSER_RULE_RHSS},
        "{std::move($1), std::move($3), std::move($5)}"}},
      {PARSER_RULE_RHSS, {{PARSER_RULE_RHS}, "{std::move($1)}"}},
      {PARSER_RULE_RHSS,
       {{PARSER_RULE_RHSS, OR, PARSER_RULE_RHS}, "{_grammar_parse::append($1, std::move($3))}"}},
      {PARSER_RULE_RHS, {{PARSER_RULE_CONTENT, CURLY_BLOCK}, "{std::move($1), std::move($2)}"}},
      {PARSER_RULE_CONTENT, {{}, "{}"}},
      {PARSER_RULE_CONTENT,
       {{PARSER_RULE_CONTENT, ID}, "{_grammar_parse::append($1, std::move($2))}"}},
      {CURLY_BLOCK, {{OPEN_CURLY, CURLY_BLOCK_CONTENT, CLOSE_CURLY}, "{$2}"}},
      {CURLY_BLOCK_CONTENT, {{}, "{\"\"}"}},
  };

  for (int tk = WS; tk < ALL; tk++)
    if (tk != OPEN_CURLY && tk != CLOSE_CURLY)
      rules.push_back({CURLY_BLOCK_CONTENT, {{CURLY_BLOCK_CONTENT, tk}, "{$1 + $2}"}});

  grammar G = grammar(types, WS, ALL, rules);
  parser_configuration conf{preamble, [&](int x) { return prints[x]; },
                            matchers, std::move(G)};
  control_automaton a(std::move(conf));
  return 0;
}

#elif 0

#include <ranges>
#include <algorithm>
#include "parser.h"

#include "control_auto.h"

int main() {
  try
  {
    std::ifstream i("self.nh5");
    parser_configuration conf = LALR_parser().parse(i);
    control_automaton a(std::move(conf));
  }
  catch (std::exception &e)
  {
    std::cout << print_exception(e);
  }
  return 0;
}

#else

#include <filesystem>

#include "parser.h"
#include "control_auto.h"
#include "parser_generator.h"

int main(int argc, char *argv[]) {
  std::string usage = "4.exe <grammar file.nh5> [<out folder = generated>]";

  if (argc != 2 && argc != 3)
  {
    std::cout << usage << std::endl;
    return 1;
  }

  const char *grammar = argv[1];
  std::string dir = "generated";
  if (argc > 2)
    dir = argv[2];

  std::filesystem::create_directories(dir);

  try
  {
    std::cout << grammar << std::endl;
    std::ifstream i(grammar);
    auto gen = parser_generator(LALR_parser().parse(i));
    gen.generate_files(dir, "parser_table.dat", "lexer.h", "parser.h");
  }
  catch (std::exception &e)
  {
    std::cout << print_exception(e);
  }
  return 0;
}

#endif
