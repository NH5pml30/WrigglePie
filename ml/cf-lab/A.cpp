#include <iostream>
#include <vector>

int main()
{
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  int noof_objs, noof_classes, noof_parts;
  std::cin >> noof_objs >> noof_classes >> noof_parts;

  std::vector<std::vector<int>> class2ids(noof_classes);
  for (int id = 0; id < noof_objs; id++)
  {
    int clazz;
    std::cin >> clazz;
    --clazz;
    class2ids[clazz].push_back(id);
  }

  std::vector<std::vector<int>> parts2ids(noof_parts);
  int part = 0;
  for (auto &class_ids : class2ids)
    for (auto id : class_ids)
    {
      parts2ids[part++].push_back(id);
      part %= noof_parts;
    }

  for (auto &part_ids : parts2ids)
  {
    std::cout << part_ids.size();
    for (auto id : part_ids)
      std::cout << ' ' << id + 1;
    std::cout << '\n';
  }
  std::cout.flush();
  return 0;
}
