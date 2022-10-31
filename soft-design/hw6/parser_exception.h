#pragma once

#include <stdexcept>
#include <format>
#include <sstream>
#include <string>
#include <concepts>
#include <functional>

#include "tokens.h"

class parser_exception : public std::runtime_error {
public:
  const tok_pos pos;

  parser_exception(const std::string &message, tok_pos pos)
      : std::runtime_error(std::format("{}: {}", pos.to_string(), message)), pos(pos) {}
};

inline void print_exception_(std::stringstream &ss, const std::exception &e, int level) {
  ss << std::string(level, '>') << e.what() << '\n';
  try {
    std::rethrow_if_nested(e);
  } catch (const std::exception &e) {
    print_exception_(ss, e, level + 1);
  } catch (...) {
  }
}

inline std::string print_exception(const std::exception &e, int level = 0) {
  std::stringstream ss;
  print_exception_(ss, e, level);
  return ss.str();
}

template<typename CRTP_child>
class errorable_state_machine_base {
protected:
  [[noreturn]] void fail_ex(auto &&ex) {
    static_assert(std::derived_from<CRTP_child, errorable_state_machine_base>);
    static_cast<CRTP_child *>(this)->state = CRTP_child::state_t::error;
    ex();
  }

  [[noreturn]] void fail(const std::exception &e) {
    struct noreturn_lambda {
      const std::exception &e;

      [[noreturn]] void operator()() {
        throw e;
      }
    };

    fail_ex(noreturn_lambda{e});
  }

  [[noreturn]] void fail_with_nested(const std::exception &e) {
    struct noreturn_lambda {
      const std::exception &e;

      [[noreturn]] void operator()() {
        std::throw_with_nested(e);
      }
    };

    fail_ex(noreturn_lambda{e});
  }

  [[noreturn]] void fail_error_state() {
    fail(std::runtime_error("Class instance called with invalid error state"));
  }
};
