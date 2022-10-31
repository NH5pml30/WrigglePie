#pragma once

#include <functional>
#include <concepts>
#include <tuple>
#include <stdexcept>
#include <optional>

#include "tokens_impl.h"

using namespace std::literals::string_view_literals;

template<typename eval_t>
struct grammar_utils_ex {
  template<typename T, typename... Types>
  struct eval_arity : eval_arity<T, eval_t, Types...> {};

  template<typename T, typename... Types>
  requires std::invocable<T, Types...>
  struct eval_arity<T, Types...> : std::integral_constant<size_t, sizeof...(Types)> {
  };

  template<typename T>
  struct action_descriptor {
    op_token::op_type type;
    std::string_view string;
    int priority;
    T action;
  };

  template<typename T>
  action_descriptor(op_token::op_type, std::string_view, int, T) -> action_descriptor<T>;

  static constexpr std::tuple actions = {
      action_descriptor{op_token::op_type::div, "/"sv, 1, std::divides<eval_t>{}},
      action_descriptor{op_token::op_type::mul, "*"sv, 1, std::multiplies<eval_t>{}},
      action_descriptor{op_token::op_type::add, "+"sv, 0, std::plus<eval_t>{}},
      action_descriptor{op_token::op_type::sub, "-"sv, 0, std::minus<eval_t>{}},
  };

  [[noreturn]] static constexpr void default_not_found_action() {
    throw std::invalid_argument("Did not match any actions");
  }

  template<size_t idx = 0>
  static constexpr auto find_action(auto &&pred, auto &&callback, auto &&not_found_action) {
    constexpr size_t ts = std::tuple_size_v<decltype(actions)>;

    if constexpr (idx < ts) {
      auto &act = std::get<idx>(actions);
      if (pred(act))
        return callback(act);
      if constexpr (idx + 1 < ts)
        return find_action<idx + 1>(pred, callback, not_found_action);
    }
    if constexpr (!std::is_same_v<decltype(not_found_action()), void>) {
      return not_found_action();
    } else {
      not_found_action();
      default_not_found_action();  // fallback to match return type
    }
  }

  static constexpr auto find_action(auto &&pred, auto &&callback) {
    return find_action(pred, callback, [] {});
  }

  static constexpr auto find_action_by_type(op_token::op_type type, auto &&callback) {
    return find_action([=](auto &&ad) { return ad.type == type; }, callback);
  }

  template<size_t arity, size_t idx = 0>
  static constexpr auto get_args(auto &&get_argument) {
    if constexpr (idx >= arity)
      return std::make_tuple();
    else
      return std::tuple_cat(get_args<arity, idx + 1>(get_argument),
                            std::make_tuple(get_argument()));
  }

  static constexpr int get_op_prior(op_token::op_type type) {
    return find_action_by_type(type, [](auto &&ad) { return ad.priority; });
  }

  struct get_prior : public gen_op_token_visitor {
    using gen_op_token_visitor::visit;

    int result{};

    void visit(brace_token &tok) override {
      result = tok.is_closing ? 1 : 0;
    }

    void visit(op_token &tok) override {
      result = 2 + get_op_prior(tok.type);
    }
  };

  static constexpr auto eval(op_token::op_type type, auto &&get_argument) {
    return find_action_by_type(type, [&]<typename T>(const action_descriptor<T> &ad) {
      // store arguments in a tuple sequentially to avoid execution sequencing problems
      return std::apply(ad.action, get_args<eval_arity<T>::value>(get_argument));
    });
  }

  template<size_t idx = 0>
  static constexpr std::optional<op_token::op_type> match_and_advance(std::string_view &sv) {
    return find_action([=](auto &&ad) { return sv.starts_with(ad.string); },
                       [&](auto &&ad) -> std::optional<op_token::op_type> {
                         sv.remove_prefix(ad.string.size());
                         return ad.type;
                       },
                       []() -> std::optional<op_token::op_type> { return {}; });
  }
};

using eval_t = double;
using grammar_utils = grammar_utils_ex<eval_t>;
