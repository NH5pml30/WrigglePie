#include <cstdio>
#include <vector>
#include <cstdint>

template<typename type>
  class array2d {
   private:
    type *Data;
    size_t Rows, Cols;

   public:
    array2d(size_t Rows, size_t Cols) : Rows(Rows), Cols(Cols) {
      Data = new type[Rows * Cols] {};
    }

    array2d(const array2d &Other) : Rows(Other.Rows), Cols(Other.Cols) {
      Data = new type[Rows * Cols];
      memcpy(Data, Other.Data, sizeof(type) * Rows * Cols);
    }

    type * operator[](size_t i) const {
      return Data + sizeof(type) * Cols * i;
    }

    type & operator=(const array2d &Other) {
      delete[] Data;
      Rows = Other.Rows, Cols = Other.Cols;
      Data = new type[Rows * Cols];
      memcpy(Data, Other.Data, sizeof(type) * Rows * Cols);
    }

    type *GetData() const {
      return Data;
    }

    size_t GetRows() const {
      return Rows;
    }

    size_t GetCols() const {
      return Cols;
    }

    ~array2d() {
      delete[] Data;
    }
  };

std::vector<size_t> digitSort(const array2d<char> &S, int k) {
  size_t m = S.GetCols(), n = S.GetRows();
  std::vector<size_t> res1(n), res2(n);

  for (size_t i = 0; i < n; i++)
    res1[i] = i;

  std::vector<size_t> *res_front = &res1, *res_back = &res2;

  for (int i = 0; i < k; i++) {
    int cnts[256] {};

    for (size_t j = 0; j < n; j++)
      cnts[(uint8_t)S[(*res_front)[j]][m - i - 1]]++;

    int global_count = 0;
    for (int j = 0; j < 256; j++) {
      int save = cnts[j];
      cnts[j] = global_count;
      global_count += save;
    }

    for (size_t j = 0; j < n; j++)
      (*res_back)[cnts[(uint8_t)S[(*res_front)[j]][m - i - 1]]++] = (*res_front)[j];
    std::swap(res_front, res_back);
  }

  return *res_front;
}

int main() {
  int n, m, k;
  scanf("%i%i%i", &n, &m, &k);
  getchar();

  array2d<char> data = array2d<char>(n, m);
  for (int i = 0; i < n; i++) {
    fread(data[i], 1, m, stdin);
    getchar();
  }

  auto res = digitSort(data, k);

  for (int i = 0; i < n; i++) {
    printf("%.*s\n", m, data[res[i]]);
  }

  return 0;
}
