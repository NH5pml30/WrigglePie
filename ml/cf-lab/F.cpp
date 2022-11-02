#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <numbers>
#include <numeric>
#include <random>
#include <ranges>
#include <set>
#include <span>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

namespace
{
  using label_t = int;
  using feature_id_t = int;
  using feature_t = int;
  using obj_t = std::vector<feature_t>;
  using labeled_obj_t = std::pair<obj_t, label_t>;

  template<typename R, typename V>
  concept range_of = std::ranges::range<R> && std::convertible_to<std::ranges::range_value_t<R>, V>;
  template<typename R, typename V>
  concept forward_range_of =
      std::ranges::forward_range<R> && std::convertible_to<std::ranges::range_value_t<R>, V>;

  void count_labels(range_of<label_t> auto &&range, std::vector<int> &counts)
  {
    std::ranges::fill(counts, 0);
    for (auto el : range)
      counts[el]++;
  }

  auto fold(std::ranges::range auto &&range)
  {
    return std::reduce(std::begin(range), std::end(range));
  }
  template<template<typename> typename T = std::vector>
  auto materialize(std::ranges::range auto &&range)
  {
    auto r = range | std::views::common;
    return T<std::ranges::range_value_t<decltype(r)>>(std::begin(r), std::end(r));
  }

  template<typename T>
  std::vector<T> partition_stg;

  auto partition(std::ranges::forward_range auto &&range, auto UnaryPredicate)
  {
    using T = std::ranges::range_value_t<decltype(range)>;

    partition_stg<T>.clear();
    if constexpr (requires { std::size(range); })
      partition_stg<T>.reserve(std::size(range));

    auto true_iter = std::begin(range);
    auto false_iter = std::back_inserter(partition_stg<T>);
    for (auto &el : range)
    {
      if (UnaryPredicate(el))
      {
        if (&el != &*true_iter)
          std::swap(*true_iter, el);
        ++true_iter;
      }
      else
      {
        *false_iter++ = std::move(el);
      }
    }
    auto save = true_iter;
    for (auto &el : partition_stg<T>)
      std::swap(*true_iter++, el);
    partition_stg<T>.clear();
    return save;
  }

  template<class From>
  struct copy_cv_impl
  {
    template<class To>
    using Apply = To;
  };
  template<class From>
  struct copy_cv_impl<const From>
  {
    template<class To>
    using Apply = const To;
  };
  template<class From>
  struct copy_cv_impl<volatile From>
  {
    template<class To>
    using Apply = volatile To;
  };
  template<class From>
  struct copy_cv_impl<const volatile From>
  {
    template<class To>
    using Apply = const volatile To;
  };
  template<class From, class To>
  using copy_cv = typename copy_cv_impl<From>::template Apply<To>;

  class DecisionTree
  {
  public:
    struct Rule
    {
      feature_id_t f_id;
      double threshold;

      bool operator()(const obj_t &object) const
      {
        return object[f_id] < threshold;
      }
    };

  private:
    template<class... Ts>
    struct overloaded : Ts...
    {
      using Ts::operator()...;
    };

    struct NodeData
    {
      int left, right;
      Rule rule;

      friend std::ostream &operator<<(std::ostream &o, NodeData nd)
      {
        return o << "Q " << nd.rule.f_id + 1 << ' ' << nd.rule.threshold << ' ' << nd.left + 1
                 << ' ' << nd.right + 1;
      }
    };
    struct LeafData
    {
      int clazz;

      friend std::ostream &operator<<(std::ostream &o, LeafData ld)
      {
        return o << "C " << ld.clazz + 1;
      }
    };

    struct DecisionTreeNode : public std::variant<NodeData, LeafData>
    {
      using base_t = std::variant<NodeData, LeafData>;
      using base_t::variant;

      friend std::ostream &operator<<(std::ostream &o, DecisionTreeNode ld)
      {
        return std::visit<std::ostream &>([&o](auto arg) -> std::ostream & { return o << arg; },
                                          (base_t &)ld);
      }
    };

    std::vector<DecisionTreeNode> nodes;
    std::vector<int> bycounts[2];
    std::vector<int> counts;

    template<typename Estor>
    int createNode(const std::vector<labeled_obj_t> &dataset, int noof_labels,
                   std::vector<std::vector<int>> &fid2inds, int ind_begin, int ind_end,
                   int max_depth, bool optimize, int depth = 0)
    {
      int noof_objs = ind_end - ind_begin;
      int noof_features = (int)fid2inds.size();

      auto get_fid_span = [&](int fid) {
        return std::span(fid2inds[fid].begin() + ind_begin, fid2inds[fid].begin() + ind_end);
      };

      count_labels(
          get_fid_span(0) | std::views::transform([&dataset](int i) { return dataset[i].second; }),
          counts);
      nodes.emplace_back(std::in_place_type<LeafData>,
                         (int)std::distance(counts.begin(), std::ranges::max_element(counts)));
      int res = (int)nodes.size() - 1;

      if (depth >= max_depth || std::ranges::count(counts, 0) == noof_labels - 1)
        return res;

      double min_val = std::numeric_limits<double>::infinity();
      Rule min_rule = {};
      int min_i_end = -1;
      {
        Estor phi[2]{};
        phi[1].set(counts);

        for (int fid = 0; fid < noof_features; fid++)
        {
          std::ranges::fill(bycounts[0], 0);
          std::ranges::copy(counts, bycounts[1].begin());

          auto xs = get_fid_span(fid) |
                    std::views::transform([&dataset, fid](int i) { return dataset[i].first[fid]; });
          auto ys = get_fid_span(fid) |
                    std::views::transform([&dataset](int i) { return dataset[i].second; });

          phi[0].reset();
          phi[1].reset();

          int i = 0;
          int i_end = 0;
          for (int i_begin = 0; i_begin < noof_objs; i_begin = i_end, i++)
          {
            feature_t f_val = xs[i_begin];
            for (i_end = i_begin; i_end < noof_objs && xs[i_end] == f_val; i_end++)
            {
              label_t label = ys[i_end];
              phi[0].change(label, bycounts[0][label]++, +1);
              phi[1].change(label, bycounts[1][label]--, -1);
            }

            if (i_end == noof_objs)
              break;

            // if (optimize && i % 3 != 0)
            //   continue;

            auto val =
                i_end * 1.0 / noof_objs * phi[0]() + (1 - i_end * 1.0 / noof_objs) * phi[1]();
            if (val < min_val)
            {
              min_val = val;
              min_rule = Rule{fid, f_val + i_end * 1.0 / noof_objs * (xs[i_end] - f_val)};
              min_i_end = i_end;
            }
          }
        }
      }

      if (min_val == std::numeric_limits<double>::infinity())
        return res;

      for (int part_fid = 0; part_fid < noof_features; part_fid++)
        if (part_fid != min_rule.f_id)
          partition(get_fid_span(part_fid),
                    [&dataset, min_rule](int ind) { return min_rule(dataset[ind].first); });

      int to_left = createNode<Estor>(dataset, noof_labels, fid2inds, ind_begin,
                                      ind_begin + min_i_end, max_depth, optimize, depth + 1);
      int to_right = createNode<Estor>(dataset, noof_labels, fid2inds, ind_begin + min_i_end,
                                       ind_end, max_depth, optimize, depth + 1);
      nodes[res] = DecisionTreeNode(std::in_place_type<NodeData>, to_left, to_right, min_rule);
      return res;
    }

  public:
    template<typename Estor>
    DecisionTree(const std::vector<labeled_obj_t> &dataset, int noof_labels,
                 std::type_identity<Estor>, int max_depth, bool optimize)
        : bycounts{std::vector<int>(noof_labels), std::vector<int>(noof_labels)},
          counts(noof_labels)
    {
      std::vector<std::vector<int>> fid2inds(dataset[0].first.size());
      for (int fid = 0; fid < (int)fid2inds.size(); fid++)
      {
        fid2inds[fid].resize(dataset.size());
        std::iota(fid2inds[fid].begin(), fid2inds[fid].end(), 0);
        std::ranges::sort(fid2inds[fid], [&dataset, fid](int i, int j) {
          return dataset[i].first[fid] < dataset[j].first[fid];
        });
      }

      nodes.reserve(dataset.size());

      createNode<Estor>(dataset, noof_labels, fid2inds, 0, (int)fid2inds[0].size(), max_depth,
                        optimize);
    }

    friend std::ostream &operator<<(std::ostream &o, const DecisionTree &tree)
    {
      o << tree.nodes.size() << '\n';
      for (auto &node : tree.nodes)
        o << node << '\n';
      return o;
    }
  };

  class GainEstor
  {
  private:
    int last_set_sum{}, last_set_sum_squares{};
    int sum{}, sum_squares{};

  public:
    void set(std::ranges::range auto &&counts)
    {
      last_set_sum = last_set_sum_squares = 0;
      for (auto cnt : counts)
      {
        last_set_sum += cnt;
        last_set_sum_squares += cnt * cnt;
      }
      reset();
    }

    void reset()
    {
      sum = last_set_sum;
      sum_squares = last_set_sum_squares;
    }

    void change(label_t, int old_cnt, int delta)
    {
      sum += delta;
      sum_squares += delta * (2 * old_cnt + delta);
    }

    double operator()() const
    {
      return 1 - sum_squares * 1.0 / (sum * sum);
    }
  };

  class EntropyEstor
  {
  private:
    std::vector<int> last_counts, counts;

  public:
    void set(std::ranges::range auto &&counts_)
    {
      last_counts = materialize(counts_);
      counts = last_counts;
    }

    void reset()
    {
      counts = last_counts;
    }

    void change(label_t label, int, int delta)
    {
      if (label + 1 > counts.size())
        counts.resize(label + 1);
      counts[label] += delta;
    }

    double operator()() const
    {
      auto sum = fold(counts);
      double res = 0;
      for (int cnt : counts)
      {
        double p = cnt * 1.0 / sum;
        res -= cnt == 0 ? 0 : p * log2(p);
      }
      return res;
    }
  };
}  // namespace

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int noof_features, noof_labels, max_depth, ntrain;
  std::cin >> noof_features >> noof_labels >> max_depth >> ntrain;

  std::vector<labeled_obj_t> train(ntrain);
  for (auto &obj : train)
  {
    obj.first.resize(noof_features);
    for (auto &feat : obj.first)
      std::cin >> feat;
    std::cin >> obj.second;
    --obj.second;
  }

  auto tree = ntrain < 1000 ? DecisionTree(train, noof_labels, std::type_identity<EntropyEstor>{},
                                           max_depth, train.size() > 3000)
                            : DecisionTree(train, noof_labels, std::type_identity<GainEstor>{},
                                           max_depth, train.size() > 3000);
  std::cout << tree << std::endl;
  return 0;
}
