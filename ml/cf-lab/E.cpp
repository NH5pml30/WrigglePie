#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
#include <numbers>
#include <numeric>
#include <string>
#include <tuple>
#include <variant>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <ranges>

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int n_classes;
  std::cin >> n_classes;

  std::vector<int> lambdas(n_classes);
  for (auto &l : lambdas)
    std::cin >> l;

  int alpha;
  std::cin >> alpha;

  using features_t = std::vector<bool>;
  using labeled_t = std::pair<features_t, int>;

  std::unordered_map<std::string, int> word2feature;
  std::vector<std::unordered_set<std::string>> class2words(n_classes);
  std::vector<int> class_words_counts(n_classes);
  std::vector<int> class_counts(n_classes);

  auto read_samples = [&word2feature, &class_words_counts, &class_counts, &class2words](
                          std::vector<labeled_t> &samples, bool generate = true) {
    int n_samples;
    std::cin >> n_samples;
    samples.resize(n_samples);

    for (int i = 0; i < n_samples; i++)
    {
      if (generate)
      {
        std::cin >> samples[i].second;
        samples[i].second--;
      }
      int n_words;
      std::cin >> n_words;

      if (generate)
      {
        class_counts[samples[i].second]++;
        class_words_counts[samples[i].second] += n_words;
      }

      for (int j = 0; j < n_words; j++)
      {
        std::string word;
        std::cin >> word;

        int feature;
        if (generate)
        {
          feature = word2feature.insert({word, (int)word2feature.size()}).first->second;
          class2words[samples[i].second].insert(word);
        }
        else
        {
          if (auto iter = word2feature.find(word); iter == word2feature.end())
            continue;
          else
            feature = iter->second;
        }
        if (samples[i].first.size() <= feature)
          samples[i].first.resize(feature + 1);
        samples[i].first[feature] = true;
      }
    }
  };

  std::vector<labeled_t> train, test;
  read_samples(train);
  read_samples(test, false);

  std::vector<std::vector<double>> pwc(word2feature.size());
  for (int word_id = 0; word_id < (int)word2feature.size(); word_id++)
  {
    pwc[word_id].resize(n_classes);
    for (int clazz = 0; clazz < n_classes; clazz++)
      pwc[word_id][clazz] = (std::ranges::count_if(train,
                                                   [word_id, clazz](auto &vec_lbl) {
                                                     return word_id < (int)vec_lbl.first.size() &&
                                                            vec_lbl.first[word_id] &&
                                                            vec_lbl.second == clazz;
                                                   }) +
                             alpha) *
                            1.0 / (class_counts[clazz] + 2 * alpha);
  }

  /*
  pwc = {
      {3.0 / 4, 1.0 / 3, 2.0 / 3},  // ant
      {1.0 / 2, 1.0 / 3, 2.0 / 3},  // emu
      {1.0 / 2, 2.0 / 3, 1.0 / 3},  // dog
      {1.0 / 4, 2.0 / 3, 1.0 / 3},  // fish
      {1.0 / 2, 1.0 / 3, 2.0 / 3},  // bird
  };

  for (int word_id = 0; word_id < (int)word2feature.size(); word_id++, std::cout << '\t')
    for (auto &&[word, id] : word2feature)
      if (id == word_id)
      {
        std::cout << word;
        break;
      }
  std::cout << '\n';
  for (int clazz = 0; clazz < n_classes; clazz++, std::cout << '\n')
    for (int word_id = 0; word_id < (int)word2feature.size(); word_id++, std::cout << '\t')
      std::cout << pwc[word_id][clazz];
  std::cout << '\n';
  */

  std::vector<double> class2p(n_classes);
  for (auto &[msg, _] : test)
  {
    double logsum = 0;
    for (int clazz = 0; clazz < n_classes; clazz++)
    {
      double val = log(class_counts[clazz] * 1.0 / train.size() * lambdas[clazz]);
      for (int word_id = 0; word_id < (int)word2feature.size(); word_id++)
        if (double p = pwc[word_id][clazz]; word_id < msg.size() && msg[word_id])
          val += log(p);
        else
          val += log(1 - p);
      class2p[clazz] = val;
      if (!std::isinf(val))
        logsum += val;
    }

    double logavg = logsum / n_classes;
    for (auto &p : class2p)
      p = exp(p - logavg);
    double sum = std::reduce(class2p.begin(), class2p.end());
    for (auto p : class2p)
      std::cout << std::setprecision(10) << p / sum << ' ';
    std::cout << '\n';
  }

  std::cout.flush();
  return 0;
}
