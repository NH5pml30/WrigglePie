#include <iostream>
#include <fstream>
#include <unordered_map>
#include <vector>
#include <unordered_set>
#include <string>
#include <ctime>
#include <sstream>
#include <algorithm>

int main()
{
  srand(time(nullptr));
  for (int i = 0; i < 1000; i++)
  {
    int n = rand() % 50 + 3, k = rand() % (n / 2) + 1;
    std::vector<std::unordered_map<char, int>> trans(n);
    std::unordered_set<int> term;
    for (int i = 0; i < k; i++)
      while (true)
      {
        if (term.insert(rand() % n).second)
          break;
      }
    int m = std::min(rand() % 100 + 5, 26 * n);
    for (int i = 0; i < m; i++)
      while (true)
      {
        int a = rand() % n, b = rand() % n;
        char c = 'a' + rand() % ('z' - 'a' + 1);
        if (trans[a].insert({c, b}).second)
          break;
      }

    std::ofstream out("fastminimization.in");
    out << trans.size() << ' ';
    out << m << ' ' << term.size() << std::endl;

    for (auto term : term)
      out << term + 1 << ' ';
    out << std::endl;

    for (int start = 0; start < n; start++)
      for (auto next : trans[start])
        out << start + 1 << ' ' << next.second + 1 << ' ' << next.first << std::endl;
    out.close();

    out = std::ofstream("minimization.in");
    out << trans.size() << ' ';
    out << m << ' ' << term.size() << std::endl;

    for (auto term : term)
      out << term + 1 << ' ';
    out << std::endl;

    for (int start = 0; start < n; start++)
      for (auto next : trans[start])
        out << start + 1 << ' ' << next.second + 1 << ' ' << next.first << std::endl;
    out.close();

    system("fastmini.exe");
    system("mini.exe");

    std::ifstream in1("fastminimization.out"), in2("minimization.out");
    out = std::ofstream("isomorphism.in");
    std::string buf;
    std::stringstream ss;
    while (in2 >> buf)
    {
      out << buf << std::endl;
      ss << buf;
    }
    if (ss.str() == "000")
      continue;
    while (in1 >> buf)
      out << buf << std::endl;
    in1.close();
    in2.close();
    out.close();

    system("iso.exe");

    in1 = std::ifstream("isomorphism.out");
    in1 >> buf;
    if (buf != "YES")
    {
      std::cout << "oh shit!!!" << std::endl;
      return 0;
    }
    std::cout << "TEST PASSED: " << i << std::endl;
  }
}
