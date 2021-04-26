/* Nikolai Kholiavin, M3238 */
#include <iostream>
#include <vector>

std::vector<int> solve_assignment_problem(const std::vector<std::vector<int>> &M) {
  int n = (int)M.size();

  std::vector<int>
      bias_row(n), bias_col(n),
      match_col2row(n, -1), match_row2col(n, -1);
  auto get_val = [&](int r, int c) { return M[r][c] + bias_row[r] + bias_col[c]; };
  for (int i = 0; i < n; i++) {
    std::vector<bool> used_row(n), used_col(n);
    std::vector<int> parent(n, -1), min_row_for_col(n, -1);
    auto update_min = [&](int added_row) {
      for (int c = 0; c < n; c++)
        if (min_row_for_col[c] == -1 || get_val(added_row, c) < get_val(min_row_for_col[c], c))
          min_row_for_col[c] = added_row;
    };

    used_row[i] = true;
    update_min(i);
    while (true) {
      int min_cost = std::numeric_limits<int>::max(), min_row = -1, min_col = -1;
      for (int c = 0; c < n; c++)
        if (!used_col[c]) {
          if (int r = min_row_for_col[c]; get_val(r, c) < min_cost) {
            min_cost = get_val(r, c);
            min_row = r;
            min_col = c;
          }
        }

      for (int r = 0; r < n; r++)
        if (used_row[r])
          bias_row[r] -= min_cost;
      for (int c = 0; c < n; c++)
        if (used_col[c])
          bias_col[c] += min_cost;

      parent[min_col] = min_row;
      if (match_col2row[min_col] == -1) {
        int col = min_col, row = min_row;
        while (true) {
          int new_col = match_row2col[row], new_row = new_col == -1 ? -1 : parent[new_col];
          match_col2row[col] = row;
          match_row2col[row] = col;
          if (row == i)
            break;
          col = new_col, row = new_row;
        }
        break;
      }

      used_col[min_col] = true;
      used_row[match_col2row[min_col]] = true;
      update_min(match_col2row[min_col]);
    }
  }

  return match_row2col;
}

int main() {
  int n;
  std::cin >> n;

  std::vector<std::vector<int>> M(n);

  for (int i = 0; i < n; i++) {
    M[i].resize(n);
    for (int j = 0; j < n; j++)
      std::cin >> M[i][j];
  }

  auto row2col = solve_assignment_problem(M);
  long long val = 0;
  for (int r = 0; r < n; r++)
    val += M[r][row2col[r]];
  std::cout << val << std::endl;
  for (int r = 0; r < n; r++)
    std::cout << r + 1 << ' ' << row2col[r] + 1 << std::endl;
  return 0;
}
