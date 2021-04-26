/* Nikolai Kholiavin, M3238 */
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iostream>
#include <utility>
#include <optional>
#include <algorithm>

namespace my_type_traits {
template <bool flag, auto if_true, auto if_false>
struct conditional : std::integral_constant<decltype(if_true), if_true> {};

template <auto if_true, auto if_false>
struct conditional<false, if_true, if_false>
    : std::integral_constant<decltype(if_false), if_false> {};

template <bool flag, auto if_true, auto if_false>
constexpr inline auto conditional_v =
    conditional<flag, if_true, if_false>::value;
} // namespace my_type_traits

namespace intrusive {
constexpr bool IS_DEBUG = true, IS_LOG = false;

template <typename... Args> inline void log(Args &&... args) {
  if constexpr (IS_LOG)
    (std::wcout << ... << args);
}

template <typename T> struct noop_t {
  T val;

  constexpr noop_t(T val = {}) noexcept : val(std::move(val)) {}
  constexpr auto operator()(...) const noexcept { return val; }
};

template <typename T> noop_t(T) -> noop_t<T>;

template <> struct noop_t<void> {
  constexpr void operator()(...) const noexcept {}
};

constexpr inline noop_t<void> noop;

struct default_tag;

// binary tree element base class, holds static methods for basic
// altering/viewing BST structure
template <typename Tag = default_tag> struct binary_tree_element {
  using bst_element_t = binary_tree_element<Tag>;
  bst_element_t *left = nullptr, *right = nullptr;
  bst_element_t *parent = nullptr;
  int8_t height_diff = 0;

  template <bst_element_t *bst_element_t::*child_f, auto if_left, auto if_right>
  static constexpr auto compare_child_v =
      my_type_traits::conditional_v<child_f == &bst_element_t::left, if_left,
                                    if_right>;

  template <bst_element_t *bst_element_t::*child_f>
  static constexpr auto other_child_v =
      compare_child_v<child_f, &bst_element_t::right, &bst_element_t::left>;

  static const bst_element_t *const &c_ref(bst_element_t *const &ref) noexcept {
    return *static_cast<const bst_element_t *const *>(&ref);
  }
  static const bst_element_t *&c_ref(bst_element_t *&ref) noexcept {
    return *const_cast<const bst_element_t **>(&ref);
  }

  template <bst_element_t *bst_element_t::*child_f>
  static void set_child(bst_element_t *node, bst_element_t *child) noexcept {
    node->*child_f = child;
    if (child != nullptr)
      child->parent = node;
  }

  static void set_child(const bst_element_t *parent,
                        const bst_element_t *&old_child,
                        bst_element_t *child) noexcept {
    old_child = child;
    if (child != nullptr)
      child->parent = const_cast<bst_element_t *>(parent);
  }
  // variant for pointer to non-const (otherwise if not careful, reference will
  // bind to a temporary)
  static void set_child(bst_element_t *parent, bst_element_t *&old_child,
                        bst_element_t *child) noexcept {
    set_child(parent, c_ref(old_child), child);
  }

  // find if `child` is left/right child of `node` & return corresponding value
  // pre: node != nullptr && `child` references one of `node`'s links to
  // children
  template <typename ArgT>
  static ArgT compare_child(const bst_element_t *node,
                            const bst_element_t *const &child, ArgT &&if_left,
                            ArgT &&if_right) noexcept {
    assert(&child == &node->left || &child == &node->right);
    return &node->left == &child ? std::forward<ArgT>(if_left)
                                 : std::forward<ArgT>(if_right);
  }
  template <typename ArgT>
  static ArgT compare_child(bst_element_t *node, bst_element_t *const &child,
                            ArgT &&if_left, ArgT &&if_right) noexcept {
    return compare_child(node, c_ref(child), std::forward<ArgT>(if_left),
                         std::forward<ArgT>(if_right));
  }

  // find if `child` is left/right child of `child->parent` & return
  // corresponding value pre: child != nullptr
  template <typename ArgT>
  static ArgT compare_child(const bst_element_t *child, ArgT &&if_left,
                            ArgT &&if_right) noexcept {
    return child->parent->left == child ? if_left : if_right;
  }

  // find if `child` is left/right child of `node` & return reference to
  // corresponing parent link pre: node != nullptr && `child` references one of
  // `node`'s links to children
  static bst_element_t *const &
  compare_child(const bst_element_t *node,
                const bst_element_t *const &child) noexcept {
    return compare_child<bst_element_t *const &>(node, child, node->left,
                                                 node->right);
  }
  static bst_element_t *&compare_child(bst_element_t *node,
                                       bst_element_t *const &child) noexcept {
    return compare_child<bst_element_t *&>(node, child, node->left,
                                           node->right);
  }

  // find if `child` is left/right child of `child->parent` & return
  // corresponding parent link pre: child != nullptr
  static bst_element_t *const &
  compare_child(const bst_element_t *child) noexcept {
    return compare_child<bst_element_t *const &>(child, child->parent->left,
                                                 child->parent->right);
  }
  static bst_element_t *&compare_child(bst_element_t *child) noexcept {
    return compare_child<bst_element_t *&>(child, child->parent->left,
                                           child->parent->right);
  }

  // find node's neighbour in BST
  // (reference to node is used only in case `*holder == nullptr`)
  // pre: holder references one of `parent`'s links to children
  template <bst_element_t *bst_element_t::*first>
  static const bst_element_t *neighbour(const bst_element_t *const &holder,
                                        const bst_element_t *parent) noexcept {
    constexpr auto second = other_child_v<first>;

    const bst_element_t *node = holder;
    if (node != nullptr && node->*first != nullptr) {
      node = node->*first;
      while (node->*second != nullptr)
        node = node->*second;
      return node;
    }

    if (parent == nullptr)
      return nullptr;
    if (node == nullptr ? &holder == &(parent->*first)
                        : node == parent->*first) {
      while (parent != nullptr && parent->*first == node) {
        node = parent;
        parent = parent->parent;
      }
    }
    return parent;
  }

  template <bst_element_t *bst_element_t::*first>
  // variant for pointer to non-const (otherwise if not careful, reference will
  // bind to a temporary)
  static bst_element_t *neighbour(bst_element_t *const &holder,
                                  bst_element_t *parent) noexcept {
    return const_cast<bst_element_t *>(neighbour<first>(c_ref(holder), parent));
  }

  // pre: node != nullptr
  template <bst_element_t *bst_element_t::*first>
  static bst_element_t *neighbour(bst_element_t *node) noexcept {
    return neighbour<first>(
        node, node->parent); // node != nullptr, so &node won't be used
  }
  template <bst_element_t *bst_element_t::*first>
  static const bst_element_t *neighbour(const bst_element_t *node) noexcept {
    return neighbour<first>(
        node, node->parent); // node != nullptr, so &node won't be used
  }

  // pre: `node` references one of `parent`'s links to children
  static const bst_element_t *next(const bst_element_t *const &node,
                                   const bst_element_t *parent) noexcept {
    return neighbour<&bst_element_t::right>(node, parent);
  }
  static bst_element_t *next(bst_element_t *const &node,
                             bst_element_t *parent) noexcept {
    return neighbour<&bst_element_t::right>(node, parent);
  }
  // pre: node != nullptr
  static const bst_element_t *next(const bst_element_t *node) noexcept {
    return neighbour<&bst_element_t::right>(node);
  }
  static bst_element_t *next(bst_element_t *node) noexcept {
    return neighbour<&bst_element_t::right>(node);
  }

  // pre: `node` references one of `parent`'s links to children
  static const bst_element_t *prev(const bst_element_t *const &node,
                                   const bst_element_t *parent) noexcept {
    return neighbour<&bst_element_t::left>(node, parent);
  }
  static bst_element_t *prev(bst_element_t *const &node,
                             bst_element_t *parent) noexcept {
    return neighbour<&bst_element_t::left>(node, parent);
  }
  // pre: node != nullptr
  static const bst_element_t *prev(const bst_element_t *node) noexcept {
    return neighbour<&bst_element_t::left>(node);
  }
  static bst_element_t *prev(bst_element_t *node) noexcept {
    return neighbour<&bst_element_t::left>(node);
  }

  // update `node`'s balance based on subtree height change
  // pre: node == nullptr || `came_from` references one of `node`'s links to
  // children
  static void on_changed_subtree(bst_element_t *node,
                                 const bst_element_t *const &came_from,
                                 int diff) noexcept {
    if (node != nullptr)
      node->height_diff += compare_child(node, came_from, +diff, -diff);
  }
  static void on_changed_subtree(bst_element_t *node,
                                 bst_element_t *const &came_from,
                                 int diff) noexcept {
    on_changed_subtree(node, c_ref(came_from), diff);
  }

  // Debug functions
  template <class Compare>
  static std::pair<size_t, size_t>
  check_tree(const bst_element_t *node, const bst_element_t *left_bound,
             const bst_element_t *right_bound, Compare &&compare) noexcept {
    if constexpr (!IS_DEBUG)
      return {0, 0};

    assert(left_bound == compare.inf && right_bound == compare.inf ||
           compare(left_bound, right_bound));
    if (node == nullptr)
      return {0, 0};
    assert(node->left == nullptr || node->left->parent == node);
    assert(node->right == nullptr || node->right->parent == node);
    assert(compare(left_bound, node));
    assert(node == compare.inf && right_bound == compare.inf ||
           compare(node, right_bound));
    auto [left_h, left_s] = check_tree(node->left, left_bound, node,
                                       std::forward<Compare>(compare));
    auto [right_h, right_s] = check_tree(node->right, node, right_bound,
                                         std::forward<Compare>(compare));
    assert(node->height_diff == left_h - right_h);
    assert(abs(node->height_diff) <= 1);
    return {std::max(left_h, right_h) + 1, left_s + right_s + 1};
  }

  template <typename T>
  static void print(const bst_element_t *node, const bst_element_t *end,
                    const std::wstring &pre = L"", bool is_left = false,
                    bool is_first = true) {
    if constexpr (!IS_DEBUG)
      return;
    if (node == nullptr)
      return;
    print<T>(node->left, end,
             pre + (is_first ? L"" : (is_left ? L"        " : L"│       ")),
             true, false);
    log(pre);
    if (!is_first)
      log(is_left ? L"┌──────" : L"└──────");
    if (node != end)
      log(L"[", static_cast<T &>(*node), ", ", node->height_diff, L"]", '\n');
    else
      log(L"[end, ", node->height_diff, L"]", '\n');
    print<T>(node->right, end,
             pre + (is_first ? L"" : (is_left ? L"│       " : L"        ")),
             false, false);
  }

  template <class Callback>
  static void const_walk(const bst_element_t *node, const bst_element_t *end, Callback &&callback)
      noexcept(noexcept(callback(std::declval<const bst_element_t*>()))) {
    if (node == nullptr)
      return;
    const_walk(node->left, end, std::forward<Callback>(callback));
    const_walk(node->right, end, std::forward<Callback>(callback));
    if (node != end)
      callback(node);
  }

  template <class Callback>
  static void walk(bst_element_t *node, const bst_element_t *end, Callback &&callback)
      noexcept(noexcept(callback(std::declval<bst_element_t*>()))) {
    const_walk(node, end, [&callback](const bst_element_t *node) {
      return callback(const_cast<bst_element_t *>(node));
    });
  }
};

// class-helper for iterating over path in BST/AVL tree
template <typename Tag, bool is_const = false> struct node_view {
  using node_t = std::conditional_t<is_const, const binary_tree_element<Tag>,
                                    binary_tree_element<Tag>>;
  using link_t = std::conditional_t<is_const, node_t *const, node_t *>;
  node_t *parent; // previous node in path (parent)
  link_t &holder; // reference to a parental link to the current node
                  // inv: holder != nullptr

  node_view(node_t *const &&node) noexcept
      : parent(node->parent), holder(c_ref(node_t::compare_child(node))) {}
  node_view(link_t &holder) noexcept : parent(holder->parent), holder(holder) {}
  node_view(node_t *parent, link_t &holder) noexcept
      : parent(parent), holder(holder) {}

  operator node_view<Tag, true>() const noexcept {
    return node_view<Tag, true>(parent, node_t::c_ref(holder));
  }

  operator node_t *() const noexcept { return holder; }

  node_t *next() const noexcept { return node_t::next(holder, parent); }
  node_t *prev() const noexcept { return node_t::prev(holder, parent); }

  template <typename ArgT>
  ArgT compare_child(ArgT &&if_left, ArgT &&if_right) const noexcept {
    assert(parent != nullptr);
    return node_t::compare_child(parent, holder, std::forward<ArgT>(if_left),
                                 std::forward<ArgT>(if_right));
  }

  link_t &compare_child() const noexcept { return holder; }

  static link_t &c_ref(binary_tree_element<Tag> *&node) noexcept {
    if constexpr (is_const)
      return node_t::c_ref(node);
    else
      return node;
  }
  static link_t &c_ref(binary_tree_element<Tag> *const &node) noexcept {
    static_assert(is_const);
    return node_t::c_ref(node);
  }

  template <binary_tree_element<Tag> *node_t::*child>
  link_t &link_child() const noexcept {
    return c_ref(holder->*child);
  }

  // update parent in case of changing path
  node_view update() const noexcept {
    if (holder != nullptr)
      return node_view(holder->parent, node_t::compare_child(holder));
    return node_view(parent, holder);
  }

  // hang `node` subtree into holder
  node_t *set(node_t *node) const noexcept {
    node_t *saved = holder;
    node_t::set_child(parent, holder, node);
    return saved;
  }

  // replace current node in `holder` with node `with`
  node_t *replace(node_t *with) const noexcept {
    with->height_diff = holder == nullptr ? 0 : holder->height_diff;
    node_t::template set_child<&node_t::left>(
        with, holder == nullptr ? nullptr : holder->left);
    node_t::template set_child<&node_t::right>(
        with, holder == nullptr ? nullptr : holder->right);
    return set(with);
  }

  void on_changed_subtree(int diff) const noexcept {
    node_t::on_changed_subtree(parent, holder, diff);
  }

  // collapse this node and hang child's subtree into holder
  // pre: other child (not child_f) is empty
  template <node_t *node_t::*child_f>
  node_t *collapse_to_child() const noexcept {
    constexpr auto child_s = node_t::template other_child_v<child_f>;

    assert(holder->*child_s == nullptr);
    node_t *deleted = set(holder->*child_f);
    on_changed_subtree(-1);
    return deleted;
  }

  // pre: at least one of children is empty
  node_t *collapse_to_child() const noexcept {
    assert(holder->left == nullptr || holder->right == nullptr);
    return holder->left == nullptr ? collapse_to_child<&node_t::right>()
                                   : collapse_to_child<&node_t::left>();
  }

  // small rotate for balancing AVL tree
  // pre: holder != nullptr && `holder`'s balance is skewed into `*second` child
  template <node_t *node_t::*first> void small_rotate() const noexcept {
    constexpr node_t *node_t::*second = node_t::template other_child_v<first>;
    constexpr int skew = node_t::template compare_child_v<first, 1, -1>;

    node_t *b = holder->*second;
    assert(holder->height_diff * skew <= -1);
    int8_t d1 = holder->height_diff, d2 = b->height_diff;
    b->height_diff =
        skew * std::max(skew * d1 + std::max(skew * d2, 0), skew * d2 - 1) +
        2 * skew;
    holder->height_diff = d1 - skew * std::min(skew * d2, 0) + skew;

    node_t::template set_child<second>(holder, b->*first);
    node_t *saved = set(b);
    node_t::template set_child<first>(b, saved);
  }

  // big rotate for balancing AVL tree
  // pre: holder != nullptr &&
  //   `holder`'s balance is heavily skewed (abs = 2) into `*second` child &&
  //   `holder->*second`'s balance is skewed (abs = 1) into `*first` child
  template <node_t *node_t::*first> void big_rotate() const noexcept {
    constexpr node_t *node_t::*second = node_t::template other_child_v<first>;
    constexpr int skew = node_t::template compare_child_v<first, 1, -1>;

    assert(holder->height_diff == -2 * skew &&
           (holder->*second)->height_diff == skew);
    node_view(holder, holder->*second).small_rotate<second>();
    small_rotate<first>();
  }

  // balance AVL tree, returns wether restructure occured
  // pre: holder != nullptr
  template <node_t *node_t::*first> bool balance() const noexcept {
    constexpr node_t *node_t::*second = node_t::template other_child_v<first>;
    constexpr int skew = node_t::template compare_child_v<first, 1, -1>;

    assert(holder != nullptr);
    if (holder->height_diff == -2 * skew) {
      if ((holder->*second)->height_diff == skew)
        big_rotate<first>();
      else
        small_rotate<first>();
      return true;
    }
    return false;
  }

  bool balance() const noexcept {
    return balance<&node_t::right>() || balance<&node_t::left>();
  }

  // top-down search of `el` starting from `holder`
  // if found el or came to empty spot, calls `found`
  // on the way back up calls `unwind` until `stop` flag is set
  // returns:
  //   if exact_match is set: the smallest element `x` on the search path so
  //   that x >= el otherwise: the smallest element `x` on the search path so
  //   that x > el
  template <bool exact_match, class Key, class Compare, class Found,
            class Unwind>
  node_t *top_down_search(const Key *el, Compare &&compare, Found &&found,
                          Unwind &&unwind, bool &stop) const
      noexcept(std::remove_reference_t<Compare>::template is_nothrow_comparable_key_v<Key>) {
    bool matched = false;
    if (holder != nullptr) {
      int next = 0;
      if (compare(holder, el))
        next = 1;
      else if (compare(el, holder)) {
        matched = true;
        next = -1;
      } else {
        if (exact_match)
          matched = true;
        else
          next = 1;
      }
      if (next != 0) {
        // const_view: parent: const node_t *, holder: const node_t *const &
        //   holder->left: node_t *const &, need const node_t *const &
        // view: parent: node_t *, node_t *&
        //   holder->left: node_t *& -- ok
        node_t *res =
            node_view(holder, next == -1 ? link_child<&node_t::left>()
                                         : link_child<&node_t::right>())
                .top_down_search<exact_match>(
                    el, std::forward<Compare>(compare),
                    std::forward<Found>(found), std::forward<Unwind>(unwind),
                    stop);
        // nothrow from this point on
        if (!stop)
          unwind(*this, stop);
        return res == nullptr ? (matched ? holder : nullptr) : res;
      }
    }
    stop = found(*this);
    return matched ? holder : nullptr;
  }

  // top-down search of `el` starting from `hint`
  // if found el or came to empty spot, calls `found`
  // on the way back up (to `holder`) calls `unwind` until `stop` flag is set
  // if `el` is the searched element and `hint` points to it, this function
  // cannot throw returns:
  //   if exact_match is set: the smallest element `x` on the search path so
  //   that x >= el otherwise: the smallest element `x` on the search path so
  //   that x > el
  // pre: `hint` is in subtree of vertex in `holder`
  template <bool exact_match, class Key, class Compare,
            class Found = const noop_t<bool> &,
            class Unwind = const noop_t<void> &>
  node_t *hinted_search(const Key *el, node_view hint, Compare &&compare,
                        Found &&found = noop_t(true),
                        Unwind &&unwind = noop) const
      noexcept(std::remove_reference_t<Compare>::template is_nothrow_comparable_key_v<Key>) {
    bool stop = true;
    std::optional<node_t *> res;
    if constexpr (exact_match && std::is_convertible_v<Key, node_t>) {
      if (el == compare.neg_inf ? hint.holder == nullptr : el == hint.holder) {
        // hint points to el, meaning that they are equivalent => found
        // (this function is noexcept in this case)
        stop = found(hint);
        res = hint.holder;
      }
    }
    if (!res.has_value())
      res = hint.top_down_search<exact_match>(
          el, std::forward<Compare>(compare), std::forward<Found>(found),
          std::forward<Unwind>(unwind), stop);

    // nothrow continues
    if (holder != hint.holder) {
      node_t *cur = hint.parent;
      while (!stop && cur != holder) {
        auto saved_parent = cur->parent;
        unwind(node_view(std::move(cur)), stop);
        cur = saved_parent;
      }
      if (!stop)
        unwind(*this, stop);
    }
    return res.value();
  }

  void unwind_insert(bool &stop) const noexcept {
    if (stop |= (holder->height_diff == 0))
      return;
    if (balance())
      stop |= holder->height_diff == 0;
    if (!stop)
      on_changed_subtree(1);
  }

  void unwind_erase(bool &stop) const noexcept {
    if (stop |= (abs(holder->height_diff) == 1))
      return;
    if (balance())
      stop |= holder->height_diff != 0;
    if (!stop)
      on_changed_subtree(-1);
  }

  // if compare throws, the insertion has no effect
  // if `hint` points to a place to insert (for example, result of
  // `try_insert`), function cannot throw
  template <class Compare>
  std::pair<node_t *, bool> insert(node_t *el, node_view hint,
                                   Compare &&compare) const
      noexcept(std::remove_reference_t<Compare>::is_nothrow_comparable_v) {
    node_t *res = nullptr;
    node_t *match = hinted_search<true>(
        el, hint, std::forward<Compare>(compare),
        // if found place to insert
        [el, &res](node_view view) {
          // came here only if comparator didn't throw & won't call comparator
          // anymore, meaning we can insert the element
          if (view.holder == nullptr) {
            view.replace(el);
            view.on_changed_subtree(1);
            res = el;
            return false;
          }
          return true;
        },
        // balance (does not throw)
        [](node_view view, bool &stop) { view.unwind_insert(stop); });

    return res == nullptr ? std::make_pair(match, false)
                          : std::make_pair(res, true);
  }

  // check if can insert element into AVL
  // returns:
  //   if can insert: insertion place view, true
  //   otherwise: element that prevented insertion view, false
  // pre: `hint` is in subtree of vertex in `holder`
  template <class Key, class Compare>
  std::pair<node_view, bool> try_insert(Key *el, node_view hint,
                                        Compare &&compare) const
      noexcept(std::remove_reference_t<Compare>::template is_nothrow_comparable_key_v<Key>) {
    std::optional<node_view> res;
    node_t *match =
        hinted_search<true>(el, hint, std::forward<Compare>(compare),
                            // if found place to insert
                            [&res](node_view view) {
                              if (view.holder == nullptr)
                                res.emplace(view);
                              return true;
                            });

    return res.has_value() ? std::make_pair(res.value(), true)
                           : std::make_pair(match->parent == nullptr
                                                ? *this
                                                : node_view(std::move(match)),
                                            false);
  }

  // pre: `hint` is in subtree of vertex in `holder`
  template <class Key, class Compare>
  node_t *lower_bound(const Key *el, node_view hint, Compare &&compare) const
      noexcept(std::remove_reference_t<Compare>::template is_nothrow_comparable_key_v<Key>) {
    return hinted_search<true>(el, hint, std::forward<Compare>(compare));
  }

  // pre: `hint` is in subtree of vertex in `holder`
  template <class Key, class Compare>
  node_t *upper_bound(const Key *el, node_view hint, Compare &&compare) const
      noexcept(std::remove_reference_t<Compare>::template is_nothrow_comparable_key_v<Key>) {
    return hinted_search<false>(el, hint, std::forward<Compare>(compare));
  }

  // find element `el` in BST
  // returns: pointer to node if found, nullptr otherwise.
  template <class Key, class Compare>
  node_t *find(const Key *el, node_view hint, Compare &&compare) const
      noexcept(std::remove_reference_t<Compare>::template is_nothrow_comparable_key_v<Key>) {
    node_t *res = nullptr;
    hinted_search<true>(el, hint, std::forward<Compare>(compare),
                        [&](node_view view) {
                          res = view.holder;
                          return true;
                        });
    return res;
  }

  // switch vertex in holder out with its successor in order
  // sets `stop` flag for unwinding
  // returns: pointer to removed vertex
  // pre: both children of vertex in `holder` & `holder` are non-empty
  template <class Compare>
  node_t *switch_for_approx(bool &stop, Compare &&compare) const noexcept {
    assert(holder->left != nullptr && holder->right != nullptr);

    // all children are filled, find next vertex from ours (leftmost in right
    // subtree)
    node_t *a;
    auto bound = compare.neg_inf;
    // compare cannot throw because -inf overrides comparator, so noexcept from
    // this point
    node_view(holder, holder->right)
        .top_down_search<true>(
            bound, std::forward<Compare>(compare),
            // parent is next(view.holder)
            [&a](node_view view) {
              a = view.parent;
              return false;
            },
            // unwind (balance)
            [this, &a, is_first = true](node_view view, bool &stop) mutable {
              if (is_first) {
                is_first = false;
                // found approximating vertex that is minimum in its subtree,
                // meaning that left child is empty, can replace this vertex
                // with right child
                view.collapse_to_child();
                // place removed vertex in place of ours
                replace(a);
                stop = false;
              } else {
                // possibly view got corrupted due to replacement. Update parent
                node_view upd_view = view.update();
                // continue balance
                upd_view.unwind_erase(stop);
              }
            },
            stop);
    return a;
  }

  // erase vertex in `holder`, sets stop for unwinding
  // returns: next element in order, if found on this stage
  // pre: `holder` != nullptr
  template <class Compare>
  node_t *erase_this(bool &stop, Compare &&compare) const noexcept {
    node_t *res;
    if (holder->left == nullptr || holder->right == nullptr) {
      // can replace this vertex with child, discarding empty
      // (this is noexcept)
      stop = false;
      bool can_find_next = holder->right != nullptr;
      collapse_to_child();
      res = can_find_next ? upper_bound(compare.neg_inf, *this,
                                        std::forward<Compare>(compare))
                          : nullptr;
    } else {
      // replace this with approximate (next in order) vertex & continue
      // balancing
      res = switch_for_approx(stop, std::forward<Compare>(compare)); // noexcept
      // All changes to parents already happened, so no corruption here
      unwind_erase(stop);
    }
    return res;
  }

  // erase `el` from AVL tree
  // if comparator throws, erasure has no effect
  // if `el` is the erased element and `hint` points to it, this function cannot
  // throw returns:
  //   if el not found, {nullptr, nullptr}
  //   otherwise, {removed element, next element in order}
  // pre: `hint` is in subtree of vertex in `holder`
  template <class Key, class Compare>
  std::pair<node_t *, node_t *> erase(Key *el, node_view hint,
                                      Compare &&compare) const
      noexcept(std::remove_reference_t<Compare>::template is_nothrow_comparable_key_v<Key>) {
    node_t *removed = nullptr;
    node_t *next = nullptr;
    hinted_search<true>(
        el, hint, std::forward<Compare>(compare),
        // if found element
        [el, &compare, &removed, &next](node_view view) {
          // if comparator throws, nothing changes in the tree structure
          // from this point noexcept
          bool stop = true;
          if (view.holder != nullptr) {
            removed = view.holder;
            next = view.erase_this(stop, std::forward<Compare>(compare));
            if (next == nullptr)
              next = view.next(); // always exists (maybe `_end`)
          }
          return stop;
        },
        // unwind (balance stage)
        [](node_view view, bool &stop) { view.unwind_erase(stop); });
    return {removed, next};
  }
};

template <typename Tag = default_tag>
using const_node_view = node_view<Tag, true>;
} // namespace intrusive

namespace intrusive {
namespace compare_traits {
template <class Compare, typename LeftT, typename RightT>
constexpr inline bool is_nothrow_comparable_ex_v = noexcept(
    std::declval<Compare>()(std::declval<LeftT>(), std::declval<RightT>()));

template <class Compare, typename T>
constexpr inline bool is_nothrow_comparable_v =
    is_nothrow_comparable_ex_v<Compare, const T &, const T &>;

template <class Compare, typename NodeT, typename T, typename Key>
constexpr inline bool is_nothrow_comparable_key_v =
    is_nothrow_comparable_ex_v<Compare, const Key &, const T &>
        &&is_nothrow_comparable_ex_v<Compare, const T &, const Key &>;
template <class Compare, typename NodeT, typename T>
constexpr inline bool is_nothrow_comparable_key_v<Compare, NodeT, T, NodeT> =
    is_nothrow_comparable_v<Compare, T>;
} // namespace compare_traits

template <typename T, class Compare = std::less<T>, typename Tag = default_tag>
class avl_tree {
private:
  using node_t = binary_tree_element<Tag>;
  using node_view_t = node_view<Tag>;
  using const_node_view_t = const_node_view<Tag>;
  node_t end_ = {nullptr, nullptr, nullptr};
  node_t *root = &end_;
  node_t *min = root;
  size_t size_ = 0;

  mutable struct comparer : public Compare {
    static constexpr node_t *neg_inf = nullptr;
    const node_t *inf;

    static constexpr bool is_nothrow_comparable_v =
        compare_traits::is_nothrow_comparable_v<Compare, T>;

    template <class Key>
    static constexpr bool is_nothrow_comparable_key_v =
        compare_traits::is_nothrow_comparable_key_v<Compare, node_t, T, Key>;

    template <class LeftT, class RightT>
    static constexpr bool is_nothrow_comparable_ex_v =
        compare_traits::is_nothrow_comparable_ex_v<Compare, LeftT, RightT>;

    comparer(Compare less, const node_t *inf)
        : Compare(std::move(less)), inf(inf) {}

    bool operator()(const node_t *left, const node_t *right) const
        noexcept(is_nothrow_comparable_v) {
      return left == neg_inf
                 ? right != neg_inf
                 : (right == inf ? left != inf
                                 : left != inf && right != neg_inf &&
                                       Compare::operator()(
                                           static_cast<const T &>(*left),
                                           static_cast<const T &>(*right)));
    }
    template <class Key>
    std::enable_if_t<!std::is_same_v<Key, node_t>, bool>
    operator()(const node_t *left, const Key *right) const
        noexcept(is_nothrow_comparable_ex_v<const T &, const Key &>) {
      return left == neg_inf ||
             (left != inf &&
              Compare::operator()(static_cast<const T &>(*left), *right));
    }
    template <class Key>
    std::enable_if_t<!std::is_same_v<Key, node_t>, bool>
    operator()(const Key *left, const node_t *right) const
        noexcept(is_nothrow_comparable_ex_v<const Key &, const T &>) {
      return right == inf ||
             (right != neg_inf &&
              Compare::operator()(*left, static_cast<const T &>(*right)));
    }
    template <class Key>
    std::enable_if_t<!std::is_same_v<Key, node_t>, bool>
    operator()(const Key *left, const T *right) const
        noexcept(is_nothrow_comparable_ex_v<const T &, const T &>) {
      return Compare::operator()(*left, *right);
    }
  } less;

public:
  static constexpr bool is_nothrow_searchable_v =
      comparer::is_nothrow_comparable_v;

  class iterator {
    friend class avl_tree;

  protected:
    const node_t *data;

    explicit iterator(const node_t *data) noexcept : data(data) {}

  public:
    using value_type = const T;
    using difference_type = std::ptrdiff_t;
    using pointer = const T *;
    using reference = const T &;
    using iterator_category = std::bidirectional_iterator_tag;

  protected:
    reference get() const noexcept { return static_cast<reference>(*data); }
    pointer get_ptr() const noexcept { return &get(); }

  public:
    iterator() = default;

    reference operator*() const noexcept { return get(); }
    pointer operator->() const noexcept { return get_ptr(); }

    bool operator==(iterator other) const noexcept {
      return data == other.data;
    }
    bool operator!=(iterator other) const noexcept {
      return data != other.data;
    }

    iterator &operator--() noexcept {
      data = node_t::prev(std::move(data));
      return *this;
    }
    iterator &operator++() noexcept {
      data = node_t::next(std::move(data));
      return *this;
    }
    iterator operator--(int) noexcept {
      iterator res(data);
      operator--();
      return res;
    }
    iterator operator++(int) noexcept {
      iterator res(data);
      operator++();
      return res;
    }
  };

private:
  const_node_view_t view_node(iterator pos) const noexcept {
    const node_t *const *holder =
        pos.data == root ? &root : &node_t::compare_child(pos.data);
    return const_node_view_t(*holder);
  }
  node_view_t view_node(iterator pos) noexcept {
    node_t *data = const_cast<node_t *>(pos.data);
    node_t **holder = pos.data == root ? &root : &node_t::compare_child(data);
    return node_view_t(*holder);
  }

  const_node_view_t view_root() const noexcept {
    return view_node(iterator(root));
  }
  node_view_t view_root() noexcept { return view_node(iterator(root)); }

  iterator iter_from_found(const node_t *node) const noexcept {
    return node == nullptr ? end() : iterator(node);
  }

  void print() const { node_t::template print<T>(root, &end_); }

  void check_tree() const {
    if (IS_DEBUG)
      assert(size_ ==
             node_t::check_tree(root, less.neg_inf, less.inf, less).second - 1);
  }

public:
  static_assert(std::is_convertible_v<T &, binary_tree_element<Tag> &>,
                "value type is not convertible to binary_tree_element");

  avl_tree(Compare less = Compare())
      noexcept(std::is_nothrow_move_constructible_v<Compare>)
      : less{std::move(less), &end_} {}
  avl_tree(avl_tree &&other) noexcept(
      std::is_nothrow_move_constructible_v<Compare>)
      : root(other.root), less{std::move(other.less.default_less), &end_} {
    node_view(other.end_.parent == nullptr ? root : other.end_.parent,
              other.end_)
        .replace(&end_);
    other.clear();
  }
  avl_tree(const avl_tree &other) = delete;
  avl_tree &operator=(avl_tree &&other)
      noexcept(std::is_nothrow_move_assignable_v<Compare>) {
    root = other.root;
    less.default_less = std::move(other.less.default_less);
    node_view(other.end_.parent == nullptr ? root : other.end_.parent,
              other.end_)
        .replace(&end_);
    other.clear();
  }
  avl_tree &operator=(const avl_tree &other) = delete;

  iterator iterator_from_ref(const T &el) const noexcept {
    return iterator(&el);
  }
  // pre: view views an existing element
  iterator iterator_from_view(const_node_view_t view) const noexcept {
    assert(view.holder != nullptr);
    return iterator(view.holder);
  }

  void clear() noexcept {
    root = &end_;
    root->left = root->right = root->parent = nullptr;
    min = root;
    size_ = 0;
  }

  template <class Key>
  std::pair<const_node_view_t, bool> try_insert(iterator hint,
                                                const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return view_root().try_insert(&el, view_node(hint), less);
  }
  template <class Key>
  std::pair<const_node_view_t, bool> try_insert(const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return try_insert(iterator(root), el);
  }

  // if comparator throws, insertion has no effect
  std::pair<iterator, bool>
  insert(T &el) noexcept(comparer::is_nothrow_comparable_v) {
    return insert(view_root(), el);
  }

  // `hint`: iterator, used as a place to start the search
  // behaviour is undefined if the right place to insert is not in subtree of
  // `hint` if compare throws, the insertion has no effect
  std::pair<iterator, bool>
  insert(iterator hint, T &el) noexcept(comparer::is_nothrow_comparable_v) {
    return insert(view_node(hint), el);
  }

  // `hint`: node_view, used as a place to start the search
  // behaviour is undefined if the right place to insert is not in subtree of
  // `hint` if compare throws, the insertion has no effect if `hint` points to a
  // place to insert (for example, result of `try_insert`), function cannot
  // throw
  std::pair<iterator, bool>
  insert(const_node_view_t hint,
         T &el) noexcept(comparer::is_nothrow_comparable_v) {
    size_++;

    node_t *&holder = *const_cast<node_t **>(&hint.holder);
    node_t *parent = const_cast<node_t *>(hint.parent);
    auto [res, inserted] =
        view_root().insert(&el, node_view_t(parent, holder), less);
    min = view_root().upper_bound(static_cast<node_t *>(nullptr), view_root(), less);
    return {iterator(res), inserted};
  }

  template <class Key>
  iterator lower_bound(const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return iter_from_found(view_root().lower_bound(&el, view_root(), less));
  }

  template <class Key>
  iterator upper_bound(const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return iter_from_found(view_root().upper_bound(&el, view_root(), less));
  }

  template <class Key>
  iterator find(const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return iter_from_found(view_root().find(&el, view_root(), less));
  }

  // returns: iterator to the next element in order
  iterator erase(iterator pos) noexcept {
    return erase(pos, static_cast<const T &>(*pos.data))
        .value()
        .first; // no compares will be executed
  }

  // if comparator throws, erasure has no effect
  // returns:
  //   if element is found, {iterator to next element, removed node}
  //   otherwise, {empty, nullptr}
  template <class Key>
  std::enable_if_t<!std::is_convertible_v<Key, iterator>,
                   std::optional<std::pair<iterator, T &>>>
  erase(const Key &el)
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return erase(iterator(root), el);
  }

  // `hint`: iterator, used as a suggestion as to where to start the search
  // behaviour is undefined if `el` is `end()`
  // if comparator throws, erasure has no effect
  // returns:
  //   if element is found, {iterator to next element, removed node}
  //   otherwise, {empty, nullptr}
  template <class Key>
  std::optional<std::pair<iterator, T &>>
  erase(iterator hint, const Key &el)
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    auto res = view_root().erase(&el, view_node(hint), less);
    if (res.first == nullptr && hint.data != root)
      // try erase from root instead
      res = view_root().erase(&el, view_root(), less);
    if (res.first != nullptr) {
      size_--;
      if (!less(min, &el) && !less(&el, min))
        min = res.second;
      return std::pair<iterator, T &>(iterator(res.second),
                                      static_cast<T &>(*res.first));
    }
    return {};
  }

  iterator begin() noexcept { return iterator(min); }
  iterator begin() const noexcept { return iterator(min); }
  iterator end() noexcept { return iterator(&end_); }
  iterator end() const noexcept { return iterator(&end_); }

  const Compare &get_comparator() const noexcept { return less; }

  template <typename Left, typename Right>
  bool call_comparator(Left &&a, Right &&b) const
      noexcept(comparer::template is_nothrow_comparable_ex_v<Left &&, Right &&>) {
    return static_cast<Compare &>(less)(std::forward<Left>(a),
                                        std::forward<Right>(b));
  }

  // pre: tree is empty
  void copy_comparator_from(const avl_tree &other)
      noexcept(std::is_nothrow_copy_assignable_v<Compare>) {
    assert(size_ == 0);
    less = comparer(other.less, &end_);
  }

  size_t size() const noexcept { return size_; }
  bool empty() const noexcept { return size_ == 0; }

  template <class Callback> void walk(Callback &&callback) const
      noexcept(noexcept(callback(std::declval<const T *>()))) {
    node_t::const_walk(root, &end_, [&](const node_t *node) {
      callback(static_cast<const T *>(node));
    });
  }

  template <class Callback> void walk(Callback &&callback)
      noexcept(noexcept(callback(std::declval<T *>()))) {
    node_t::walk(root, &end_,
                 [&](node_t *node) { callback(static_cast<T *>(node)); });
  }
};
} // namespace intrusive

#include <iostream>
#include <set>
#include <vector>
#include <map>

struct node : intrusive::binary_tree_element<>
{
  int index;
  static std::map<std::pair<int, int>, bool> cache;
  static int counter;

  node(int index) noexcept : index(index) {}

  bool operator<(const node &other) const
  {
    int x = index, y = other.index;

    bool inv = false;
    if (x > y)
    {
      std::swap(x, y);
      inv = true;
    }

    if (auto it = cache.find({x, y}); it != cache.end())
      return it->second ^ inv;

    if (counter++ >= 10000)
      throw "up";
    std::cout << 1 << ' ' << x + 1 << ' ' << y + 1 << std::endl;
    std::cout.flush();
    std::string res;
    std::cin >> res;
    cache[std::make_pair(x, y)] = (res == "YES") ^ inv;
    return res == "YES";
  }
};

int node::counter = 0;
std::map<std::pair<int, int>, bool> node::cache;

int main()
{
  // set probably uses comparator only on insert,
  // and insertion is the same with non-transitive relation:
  // if x > cur, then:
  //   if x > last, then can insert after last, ok
  //   if x < last, then at some point in [cur, last) there is 
  //     conversion from > to < (function is continuous on {0, 1}).
  //   meaning that can insert into [cur, last]
  // x < cur is the same.
  // That means basic binary search works
  // Balancing does not use comparator and preserve order, which is the only thing we care about
  intrusive::avl_tree<node> tree;

  int N;
  std::cin >> N;

  std::vector<node> nodes;
  nodes.reserve(N);

  for (int i = 0; i < N; i++)
  {
    nodes.push_back(node(i));
    tree.insert(nodes.back());
  }

  std::cout << 0 << ' ';
  for (auto &el : tree)
    std::cout << el.index + 1 << ' ';
  std::cout << std::endl;

  return 0;
}
