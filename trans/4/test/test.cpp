#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <sstream>
#include <vector>
#include <format>
#include <fstream>
#include <map>
#include <filesystem>

#include "../2/generated/parser.h"

#include <array>

std::pair<std::string, std::unique_ptr<Tree>> tests[] = {
  {
    "a|b",
    NodeBuilder("E").add(
      NodeBuilder("E").add(
        NodeBuilder("T").add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).finish()
      ).finish()
    ).add(
      NodeBuilder("|").finish()
    ).add(
      NodeBuilder("T").add(
        NodeBuilder("F").add(
          NodeBuilder("n").finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "a&b",
    NodeBuilder("E").add(
      NodeBuilder("T").add(
        NodeBuilder("F").add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).add(
          NodeBuilder("&").finish()
        ).add(
          NodeBuilder("n").finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "a^b",
    NodeBuilder("E").add(
      NodeBuilder("T").add(
        NodeBuilder("T").add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).finish()
      ).add(
        NodeBuilder("^").finish()
      ).add(
        NodeBuilder("F").add(
          NodeBuilder("n").finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "!a",
    NodeBuilder("E").add(
      NodeBuilder("T").add(
        NodeBuilder("F").add(
          NodeBuilder("G").add(
            NodeBuilder("!").finish()
          ).add(
            NodeBuilder("n").finish()
          ).finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "a|b^c&d",
    NodeBuilder("E").add(
      NodeBuilder("E").add(
        NodeBuilder("T").add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).finish()
      ).finish()
    ).add(
      NodeBuilder("|").finish()
    ).add(
      NodeBuilder("T").add(
        NodeBuilder("T").add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).finish()
      ).add(
        NodeBuilder("^").finish()
      ).add(
        NodeBuilder("F").add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).add(
          NodeBuilder("&").finish()
        ).add(
          NodeBuilder("n").finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "a&b^c|d",
    NodeBuilder("E").add(
      NodeBuilder("E").add(
        NodeBuilder("T").add(
          NodeBuilder("T").add(
            NodeBuilder("F").add(
              NodeBuilder("F").add(
                NodeBuilder("n").finish()
              ).finish()
            ).add(
              NodeBuilder("&").finish()
            ).add(
              NodeBuilder("n").finish()
            ).finish()
          ).finish()
        ).add(
          NodeBuilder("^").finish()
        ).add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).finish()
      ).finish()
    ).add(
      NodeBuilder("|").finish()
    ).add(
      NodeBuilder("T").add(
        NodeBuilder("F").add(
          NodeBuilder("n").finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "!!!!!!a",
    NodeBuilder("E").add(
      NodeBuilder("T").add(
        NodeBuilder("F").add(
          NodeBuilder("G").add(
            NodeBuilder("!").finish()
          ).add(
            NodeBuilder("G").add(
              NodeBuilder("!").finish()
            ).add(
              NodeBuilder("G").add(
                NodeBuilder("!").finish()
              ).add(
                NodeBuilder("G").add(
                  NodeBuilder("!").finish()
                ).add(
                  NodeBuilder("G").add(
                    NodeBuilder("!").finish()
                  ).add(
                    NodeBuilder("G").add(
                      NodeBuilder("!").finish()
                    ).add(
                      NodeBuilder("n").finish()
                    ).finish()
                  ).finish()
                ).finish()
              ).finish()
            ).finish()
          ).finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "a|(b|c)^!!(c|e)&!(d|e)",
    NodeBuilder("E").add(
      NodeBuilder("E").add(
        NodeBuilder("T").add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).finish()
      ).finish()
    ).add(
      NodeBuilder("|").finish()
    ).add(
      NodeBuilder("T").add(
        NodeBuilder("T").add(
          NodeBuilder("F").add(
            NodeBuilder("G").add(
              NodeBuilder("(").finish()
            ).add(
              NodeBuilder("E").add(
                NodeBuilder("E").add(
                  NodeBuilder("T").add(
                    NodeBuilder("F").add(
                      NodeBuilder("n").finish()
                    ).finish()
                  ).finish()
                ).finish()
              ).add(
                NodeBuilder("|").finish()
              ).add(
                NodeBuilder("T").add(
                  NodeBuilder("F").add(
                    NodeBuilder("n").finish()
                  ).finish()
                ).finish()
              ).finish()
            ).add(
              NodeBuilder(")").finish()
            ).finish()
          ).finish()
        ).finish()
      ).add(
        NodeBuilder("^").finish()
      ).add(
        NodeBuilder("F").add(
          NodeBuilder("F").add(
            NodeBuilder("G").add(
              NodeBuilder("!").finish()
            ).add(
              NodeBuilder("G").add(
                NodeBuilder("!").finish()
              ).add(
                NodeBuilder("G").add(
                  NodeBuilder("(").finish()
                ).add(
                  NodeBuilder("E").add(
                    NodeBuilder("E").add(
                      NodeBuilder("T").add(
                        NodeBuilder("F").add(
                          NodeBuilder("n").finish()
                        ).finish()
                      ).finish()
                    ).finish()
                  ).add(
                    NodeBuilder("|").finish()
                  ).add(
                    NodeBuilder("T").add(
                      NodeBuilder("F").add(
                        NodeBuilder("n").finish()
                      ).finish()
                    ).finish()
                  ).finish()
                ).add(
                  NodeBuilder(")").finish()
                ).finish()
              ).finish()
            ).finish()
          ).finish()
        ).add(
          NodeBuilder("&").finish()
        ).add(
          NodeBuilder("G").add(
            NodeBuilder("!").finish()
          ).add(
            NodeBuilder("G").add(
              NodeBuilder("(").finish()
            ).add(
              NodeBuilder("E").add(
                NodeBuilder("E").add(
                  NodeBuilder("T").add(
                    NodeBuilder("F").add(
                      NodeBuilder("n").finish()
                    ).finish()
                  ).finish()
                ).finish()
              ).add(
                NodeBuilder("|").finish()
              ).add(
                NodeBuilder("T").add(
                  NodeBuilder("F").add(
                    NodeBuilder("n").finish()
                  ).finish()
                ).finish()
              ).finish()
            ).add(
              NodeBuilder(")").finish()
            ).finish()
          ).finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "a|b|c|d",
    NodeBuilder("E").add(
      NodeBuilder("E").add(
        NodeBuilder("E").add(
          NodeBuilder("E").add(
            NodeBuilder("T").add(
              NodeBuilder("F").add(
                NodeBuilder("n").finish()
              ).finish()
            ).finish()
          ).finish()
        ).add(
          NodeBuilder("|").finish()
        ).add(
          NodeBuilder("T").add(
            NodeBuilder("F").add(
              NodeBuilder("n").finish()
            ).finish()
          ).finish()
        ).finish()
      ).add(
        NodeBuilder("|").finish()
      ).add(
        NodeBuilder("T").add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).finish()
      ).finish()
    ).add(
      NodeBuilder("|").finish()
    ).add(
      NodeBuilder("T").add(
        NodeBuilder("F").add(
          NodeBuilder("n").finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "a^b^c^d",
    NodeBuilder("E").add(
      NodeBuilder("T").add(
        NodeBuilder("T").add(
          NodeBuilder("T").add(
            NodeBuilder("T").add(
              NodeBuilder("F").add(
                NodeBuilder("n").finish()
              ).finish()
            ).finish()
          ).add(
            NodeBuilder("^").finish()
          ).add(
            NodeBuilder("F").add(
              NodeBuilder("n").finish()
            ).finish()
          ).finish()
        ).add(
          NodeBuilder("^").finish()
        ).add(
          NodeBuilder("F").add(
            NodeBuilder("n").finish()
          ).finish()
        ).finish()
      ).add(
        NodeBuilder("^").finish()
      ).add(
        NodeBuilder("F").add(
          NodeBuilder("n").finish()
        ).finish()
      ).finish()
    ).finish()
  },
  {
    "a&b&c&d",
    NodeBuilder("E").add(
      NodeBuilder("T").add(
        NodeBuilder("F").add(
          NodeBuilder("F").add(
            NodeBuilder("F").add(
              NodeBuilder("F").add(
                NodeBuilder("n").finish()
              ).finish()
            ).add(
              NodeBuilder("&").finish()
            ).add(
              NodeBuilder("n").finish()
            ).finish()
          ).add(
            NodeBuilder("&").finish()
          ).add(
            NodeBuilder("n").finish()
          ).finish()
        ).add(
          NodeBuilder("&").finish()
        ).add(
          NodeBuilder("n").finish()
        ).finish()
      ).finish()
    ).finish()
  },
};

std::vector<std::string> fails = {"aa", "a`", "a=b", "~a", "a||b", "(a|b|c", "a|b|c)"};

bool test() {
  int n_tests = sizeof(tests) / sizeof(tests[0]) + (int)fails.size();
  auto out_passed = [n_tests](int passed) { std::cout << "Passed/all: " << passed << '/' << n_tests << std::endl; };

  int passed = 0;
  for (auto &&[input, expected] : tests)
  {
    auto s = std::istringstream(input);
    std::unique_ptr<Tree> actual;
    try
    {
      actual = LALR_parser().parse(s);
      // std::cout << "  {\n    \"" << input << "\",\n" << actual->to_builder_str(2) << "\n  }," << std::endl;
    }
    catch (std::exception &e)
    {
      std::cout << "FAIL:\n"
                << "  no error expected for input \"" << input << "\", got : " << print_exception(e);
      return false;
    }

    {
      std::ofstream fexpected("expected.dot");
      fexpected << expected->to_dot();
    }
    if (*actual != *expected)
    {
      std::cout << "FAIL:\n"
                << "  expected <expected.dot>, actual <actual.dot> for input \"" << input
                << "\"\n";
      std::ofstream factual("actual.dot");
      factual << actual->to_dot();
      out_passed(passed);
      return false;
    }
    system(std::format("\"C:\\Program Files\\Graphviz\\bin\\dot.exe\" -Tpng expected.dot -o test{}.png", passed).c_str());
    passed++;
  }

  for (auto &input : fails)
  {
    bool caught = false;
    try
    {
      auto s = std::istringstream(input);
      LALR_parser().parse(s);
    }
    catch (std::exception &)
    {
      caught = true;
    }
    if (!caught)
    {
      std::cout << "FAIL:\n"
                << "  error expected for input \"" << input << "\"\n";
      out_passed(passed);
      return false;
    }
    passed++;
  }
  std::cout << "SUCCESS\n";
  out_passed(passed);
  return true;
}

int main() {
  // std::filesystem::current_path("../2/");

  return test() ? 0 : 1;
}
