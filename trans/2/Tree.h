#pragma once

#include <algorithm>
#include <concepts>
#include <functional>
#include <memory>
#include <numeric>
#include <sstream>
#include <string>
#include <vector>

class Tree
{
public:
  Tree() = default;

private:
  std::string node;
  std::vector<std::unique_ptr<Tree>> children;

  int to_dot_(std::stringstream &ss, int id = 0) const
  {
    int next_id = id;
    ss << std::format("  \"{}\" [label=\"{}\"{}]\n", id, node,
                      (!node.empty() && !std::isupper(node[0])) ? " shape=box" : "");
    for (auto &child : children)
    {
      ss << std::format("  \"{}\" -> \"{}\"\n", id, ++next_id);
      next_id = child->to_dot_(ss, next_id);
    }
    return next_id;
  }

  void add_indent(std::stringstream &res, int indent) const
  {
    for (int i = 0; i < indent; i++)
      res << "  ";
  }

  void to_builder_str_(std::stringstream &res, int indent) const
  {
    add_indent(res, indent);
    res << std::format("NodeBuilder(\"{}\")", node);
    for (auto &child : children)
    {
      res << ".add(\n";
      child->to_builder_str_(res, indent + 1);
      res << '\n';
      add_indent(res, indent);
      res << ')';
    }
    res << ".finish()";
  }

public:
  Tree(std::string node) : node(std::move(node)) {}

  Tree &add(std::unique_ptr<Tree> &&child)
  {
    children.push_back(std::move(child));
    return *this;
  }

  std::string to_dot() const
  {
    std::stringstream ss;
    ss << "digraph parse_tree {\n";
    to_dot_(ss);
    ss << "}\n";
    return ss.str();
  }

  std::string to_builder_str(int indent) const
  {
    std::stringstream res;
    to_builder_str_(res, indent);
    return res.str();
  }

  bool operator==(const Tree &other) const
  {
    return node == other.node &&
           std::equal(children.begin(), children.end(), other.children.begin(),
                      other.children.end(), [](auto &ptr1, auto &ptr2) { return *ptr1 == *ptr2; });
  }

  bool operator!=(const Tree &other) const
  {
    return !operator==(other);
  }
};

struct NodeBuilder
{
  std::unique_ptr<Tree> tree;

  NodeBuilder(std::string node) : tree(std::make_unique<Tree>(std::move(node))) {}

  NodeBuilder &add(std::unique_ptr<Tree> &&child) &
  {
    tree->add(std::move(child));
    return *this;
  }

  NodeBuilder &add(std::string empty_child_name) &
  {
    tree->add(NodeBuilder(empty_child_name).finish());
    return *this;
  }

  template<typename T>
  NodeBuilder &&add(T &&el) &&
  {
    return std::move(this->add(std::forward<T>(el)));
  }

  std::unique_ptr<Tree> finish() &&
  {
    return std::move(tree);
  }
};

inline std::unique_ptr<Tree> node(std::string node)
{
  return NodeBuilder(std::move(node)).finish();
}
