#include <algorithm>
#include <cassert>
#include <cmath>
#include <functional>
#include <initializer_list>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <numbers>
#include <numeric>
#include <optional>
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
#include <array>
#include <valarray>

// Multi-dimensional array class
template<class T, size_t DimsNum>
class array
{
  template<class TOther, size_t DimsNumOther>
  friend class array;

private:
  T Vector;
  std::array<int, DimsNum> Prods, Dims;  // Array dimensions & products

  /* Get element from template pack cooridnates function.
   * ARGUMENTS:
   *  - current offset sum:
   *      int Off;
   * RETURNS:
   *   (typename T::value_type &) element.
   */
  template<size_t Depth = 0, typename... TArgs>
  typename T::value_type &Get(int Off)
  {
    return Vector[Off];
  } /* End of 'Get' function */

  /* Get element from template pack cooridnates function.
   * ARGUMENTS:
   *  - current offset sum:
   *      int Off;
   *  - first coordinate:
   *      int X;
   *  - other coordinates:
   *      TArgs ...Args;
   * RETURNS:
   *   (typename T::value_type &) element.
   */
  template<size_t Depth = 0, typename... TArgs>
  typename T::value_type &Get(int Off, int X, TArgs... Args)
  {
    return Get<Depth + 1>(Off * Dims[Depth] + X, Args...);
  } /* End of 'Get' function */

  /* Recount prefix size products function.
   * ARGUMENTS: None.
   * RETURNS:
   *   (int) overall product.
   */
  int RecountProds()
  {
    int prod = 1;
    for (size_t i = 0; i < DimsNum; i++)
      Prods[DimsNum - i - 1] = prod, prod *= Dims[DimsNum - i - 1];
    return prod;
  } /* End of 'RecountProds' function */

public:
  /* Class default constructor */
  array() : Dims(), Prods() {} /* End of 'array' function */

  /* Class constructor.
   * ARGUMENTS:
   *  - array dimensions:
   *      const std::array<int, DimsNum> &Dims;
   *  - array data:
   *      T data;
   */
  array(const std::array<int, DimsNum> &Dims, T data = {})
      : Dims(Dims), Prods(), Vector(std::move(data))
  {
    int prod = RecountProds();
    if (Vector.empty())
      Vector.resize(prod);
    else
      assert(Vector.size() == prod);
  } /* End of 'array' function */

  /* Class copy constructor */
  template<class TOther>
  array(const array<TOther, DimsNum> &Arr) : Dims(Arr.Dims), Prods(Arr.Prods), Vector(Arr.Vector)
  {
  } /* End of 'array' function */

  /* Assign another array function.
   * ARGUMENTS:
   *  - array:
   *      const array<TOther, DimsNum> &Arr;
   * RETURNS:
   *   (array &) self-reference.
   */
  template<class TOther>
  array &operator=(const array<TOther, DimsNum> &Arr)
  {
    Dims = Arr.Dims, Prods = Arr.Prods;
    Vector = Arr.Vector;

    return *this;
  } /* End of 'operator=' function */

  /* Resize array function.
   * ARGUMENTS:
   *  - new array dimensions:
   *      const std::array<int, DimsNum> &NewDims;
   * RETURNS: None.
   */
  void Resize(const std::array<int, DimsNum> &NewDims)
  {
    bool changed = false;

    for (int i = 0; i < DimsNum; i++)
      if (Dims[i] != NewDims[i])
      {
        changed = true;
        break;
      }

    Dims = NewDims;

    int prod = RecountProds();
    if (changed)
      Vector.resize(prod);
  } /* End of 'Resize' function */

  /* Get vector function.
   * ARGUMENTS: None.
   * RETURNS:
   *   (const T &) vector.
   */
  const T &GetVector() const
  {
    return Vector;
  } /* End of 'GetVector' function */

  /* Get vector function.
   * ARGUMENTS: None.
   * RETURNS:
   *   (const T &) vector.
   */
  T &GetVector()
  {
    return Vector;
  } /* End of 'GetVector' function */

  /* Obtain size on depth D function.
   * ARGUMENTS:
   *  - depth:
   *      int D;
   * RETURNS:
   *   (size_t) result size on depth D.
   */
  int GetSize(int D) const
  {
    return Dims[D];
  } /* End of 'GetSize' function */

  /* Obtain overall size function.
   * ARGUMENTS:
   *  - depth:
   *      int D;
   * RETURNS:
   *   (size_t) result size on depth D.
   */
  int GetSize() const
  {
    return (int)Vector.size();
  } /* End of 'GetSize' function */

  /* Obtain number of dimensions function.
   * ARGUMENTS: None.
   * RETURNS:
   *   (size_t) number of dimensions.
   */
  size_t GetDims() const
  {
    return DimsNum;
  } /* End of 'GetDims' function */

  /* Convert to corresponding pointer type function.
   * ARGUMENTS: None.
   * RETURNS:
   *   (operator typename T::value_type *) result pointer of type typename T::value_type *.
   */
  operator typename T::value_type *() const
  {
    return Vector.data();
  } /* End of 'operator typename T::value_type *' function */

  /* Get element from coordinates function.
   * ARGUMENTS:
   *  - coordinates:
   *      int X, TArgs... Args;
   * RETURNS:
   *   (typename T::value_type &) element.
   */
  template<typename... TArgs>
  std::enable_if_t<sizeof...(TArgs) + 1 == DimsNum, typename T::value_type> &operator()(
      int X, TArgs... Args)
  {
    return Get(0, X, Args...);
  } /* End of 'operator()' function */

  /* Get element from coordinates function.
   * ARGUMENTS:
   *  - coordinates:
   *      const std::array<int, DimsNum> &Coords;
   * RETURNS:
   *   (typename T::value_type &) element.
   */
  typename T::value_type &operator()(const std::array<int, DimsNum> &Coords)
  {
    int off = 0;
    for (int i = 0; i < DimsNum; i++)
      off += Prods[i] * Coords[i];
    return Vector[off];
  } /* End of 'operator()' function */

  /* Swap data with another array function.
   * ARGUMENTS:
   *  - another array:
   *      array &Other;
   * RETURNS: None.
   */
  void swap(array &Other)
  {
    Vector.swap(Other.Vector);
    Prods.swap(Other.Prods);
    Dims.swap(Other.Dims);
  } /* End of 'swap' function */

  /* Class destructor */
  ~array() {} /* End of 'array' function */
}; /* End of 'array' class */

using array_t = array<std::vector<double>, 3>;

/* Neural network abstract layer class */
class layer
{
public:
  array_t Input;   // neuron activations
  array_t Output;  // result before activation function

  /* Class default constructor */
  layer() = default;

  /* Count result function.
   * ARGUMENTS:
   *  - input neurons values:
   *      const array_t &Input;
   *  - output neurons values:
   *      array_t &Output;
   * RETURNS: None.
   */
  void Forward(const array_t &Input, array_t &Output)
  {
    this->Input = Input;
    ForwardImpl();
    Output = this->Output;
  } /* End of 'Forward' function */

  /* Count result implementation function.
   * ARGUMENTS: None.
   * RETURNS: None.
   */
  virtual void ForwardImpl() = 0;

  /* Backward pass & count derivatives function.
   * ARGUMENTS:
   *  - [inout] derivative of cost function wrt to next layer input (this layer output):
   *      array_t &DerWrtInput;
   * RETURNS: None.
   */
  virtual void Backward(array_t &DerWrtInput) = 0;

  /* Class destructor */
  virtual ~layer() = default; /* End of '~layer' function */

  /* Print layer info to output function.
   * ARGUMENTS:
   *  - output stream:
   *      std::ostream &o;
   * RETURNS: None.
   */
  virtual std::ostream &Print(std::ostream &o)
  {
    return o;
  } /* End of 'Print' function */
}; /* End of 'layer' class */

// Padding enum
enum class padding
{
  mirror,
  extend,
  cyclic
}; /* End of 'padding' enum class */

/* Neural network convolution layer class */
template<padding P>
class layer_conv : public layer
{
public:
private:
  int Padding,   // padding size
    StrideX, StrideY; // stride
  array<std::vector<double>, 4> Kernel;       // kernel coefficients (old_depth x new_depth x height x width)
  array<std::vector<double>, 4> DerWrtKernel; // derivatives of cost function wrt kernel

  /* Map padded pixel coordinate to source function.
   * ARGUMENTS:
   *  - coord:
   *      int C;
   *  - dimension:
   *      int Size;
   * RETURNS:
   *   (int) source coordinate.
   */
  int GetSource(int C, int Size)
  {
    C -= Padding;
    auto roll_size = [](int C, int Size) {
      while (C < 0)
        C += Size;
      return C % Size;
    };
    if constexpr (P == padding::cyclic)
      return roll_size(C, Size);
    else if constexpr (P == padding::mirror)
    {
      int med = roll_size(C, 2 * Size - 2);
      return med >= Size ? 2 * Size - 2 - med : med;
    }
    else /* if constexpr (P == padding::extend) */
      return std::clamp(C, 0, Size - 1);
  } /* End of 'GetSource' function */

  /* Get input pixel (with padding) from coordinates function.
   * ARGUMENTS:
   *  - image depth:
   *      int Z;
   *  - y & x coordinates:
   *      int Y, X;
   * RETURNS:
   *   (double) result.
   */
  double GetPixel(int Z, int Y, int X)
  {
    return Input(Z, GetSource(Y, Input.GetSize(1)), GetSource(X, Input.GetSize(2)));
  } /* End of 'GetPixel' function */

  /* Convolve template function.
   * ARGUMENTS:
   *  - input neurons values:
   *      const array_t &Input;
   *  - output neurons values:
   *      array_t &Output;
   * RETURNS: None.
   */
  void ConvolveStub(auto &&action)
  {
    int InputH = Input.GetSize(1);
    int InputW = Input.GetSize(2);
    int KernelH = Kernel.GetSize(2);
    int KernelW = Kernel.GetSize(3);

    for (int i = 0; i < Output.GetSize(0); i++)      // Out depth/kernel out depth
      for (int j = 0; j < Output.GetSize(1); j++)    // Out height
        for (int k = 0; k < Output.GetSize(2); k++)  // Out width
        {
          int toplefty = std::min(j * StrideY, InputH + 2 * Padding - KernelH),
              topleftx = std::min(k * StrideX, InputW + 2 * Padding - KernelW);

          for (int l = 0; l < KernelH; l++)               // Kernel height
            for (int m = 0; m < KernelW; m++)             // Kernel width
              for (int n = 0; n < Input.GetSize(0); n++)  // In depth/kernel in depth
                action(i, j, k, n, toplefty, l, topleftx, m);
        }
  } /* End of 'ConvolveStub' function */

public:
  /* Class constructor.
   * ARGUMENTS:
   *  - stride:
   *      int StrideX, StrideY;
   *  - padding:
   *      int Padding;
   *  - the kernel:
   *      array_t Kernel.
   */
  layer_conv(int StrideX, int StrideY, int Padding,
             array<std::vector<double>, 4> Kernel)
      : StrideX(StrideX), StrideY(StrideY), Padding(Padding), Kernel(std::move(Kernel))
  {
  } /* End of 'layer_conv' function */

  /* Count result implementation function.
   * ARGUMENTS: None.
   * RETURNS: None.
   */
  void ForwardImpl() override
  {
    int InputH = Input.GetSize(1);
    int InputW = Input.GetSize(2);
    int EndD = Kernel.GetSize(0);
    int KernelH = Kernel.GetSize(2);
    int KernelW = Kernel.GetSize(3);
    Output.Resize({EndD, (InputH + 2 * Padding - KernelH) / StrideY + 1,
                   (InputW + 2 * Padding - KernelW) / StrideX + 1});

    if ((Output.GetSize(1) - 1) * StrideY + KernelH != InputH + 2 * Padding ||
        (Output.GetSize(2) - 1) * StrideX + KernelW != InputW + 2 * Padding)
      throw "up";

    ConvolveStub([this](int out_depth, int out_y, int out_x, int in_depth, int toplefty, int kern_y,
                        int topleftx, int kern_x) {
      Output(out_depth, out_y, out_x) += Kernel(out_depth, in_depth, kern_y, kern_x) *
                                         GetPixel(in_depth, toplefty + kern_y, topleftx + kern_x);
    });
  } /* End of 'ForwardImpl' function */

  /* Backward pass & count derivatives function.
   * ARGUMENTS:
   *  - [inout] derivative of cost function wrt to next layer input (this layer output):
   *      array_t &DerWrtInput;
   * RETURNS: None.
   */
  void Backward(array_t &DerWrtInput) override
  {
    // Count derivatives wrt result
    array_t &der_wrt_result = DerWrtInput;

    // Count derivatives wrt input & kernels
    array_t der_wrt_input({Input.GetSize(0), Input.GetSize(1), Input.GetSize(2)});
    DerWrtKernel.Resize(
        {Kernel.GetSize(0), Kernel.GetSize(1), Kernel.GetSize(2), Kernel.GetSize(3)});

    ConvolveStub([this, &der_wrt_input, &der_wrt_result](int out_depth, int out_y, int out_x,
                                                         int in_depth, int toplefty, int kern_y,
                                                         int topleftx, int kern_x) {
      der_wrt_input(in_depth, GetSource(toplefty + kern_y, Input.GetSize(1)),
                    GetSource(topleftx + kern_x, Input.GetSize(2))) +=
          der_wrt_result(out_depth, out_y, out_x) * Kernel(out_depth, in_depth, kern_y, kern_x);
      DerWrtKernel(out_depth, in_depth, kern_y, kern_x) +=
          der_wrt_result(out_depth, out_y, out_x) *
          GetPixel(in_depth, toplefty + kern_y, topleftx + kern_x);
    });

    // Change to new derivatives for back propagation
    DerWrtInput.swap(der_wrt_input);
  } /* End of 'Backward' function */

  /* Print layer info to output function.
   * ARGUMENTS:
   *  - output stream:
   *      std::ostream &o;
   * RETURNS: None.
   */
  std::ostream &Print(std::ostream &o) override
  {
    for (auto el : DerWrtKernel.GetVector())
      o << el << ' ';
    return o;
  }
}; /* End of 'layer_conv' class */

/* Neural network bias layer class */
class layer_bias : public layer
{
public:
private:
  std::vector<double> Bias; // bias for each depth
  std::vector<double> DerWrtBias;  // derivatives of cost function wrt bias

public:
  /* Class constructor.
   * ARGUMENTS:
   *  - bias for each depth:
   *      std::vector<double> bias.
   */
  layer_bias(std::vector<double> Bias) : Bias(std::move(Bias))
  {
  } /* End of 'layer_bias' function */

  /* Count result implementation function.
   * ARGUMENTS: None.
   * RETURNS: None.
   */
  void ForwardImpl() override
  {
    Output = Input;
    for (int depth = 0; depth < Output.GetSize(0); depth++)
      for (int y = 0; y < Output.GetSize(1); y++)
        for (int x = 0; x < Output.GetSize(2); x++)
          Output(depth, y, x) += Bias[depth];
  } /* End of 'ForwardImpl' function */

  /* Backward pass & count derivatives function.
   * ARGUMENTS:
   *  - [inout] derivative of cost function wrt to next layer input (this layer output):
   *      array_t &DerWrtInput;
   * RETURNS: None.
   */
  void Backward(array_t &DerWrtInput) override
  {
    // no change to derivative wrt input
    DerWrtBias.resize(Bias.size());
    for (int z = 0; z < DerWrtInput.GetSize(0); z++)
    {
      double sum = 0;
      for (int y = 0; y < DerWrtInput.GetSize(1); y++)
        for (int x = 0; x < DerWrtInput.GetSize(2); x++)
          sum += DerWrtInput(z, y, x);
      DerWrtBias[z] = sum;
    }
  } /* End of 'Backward' function */

  /* Print layer info to output function.
   * ARGUMENTS:
   *  - output stream:
   *      std::ostream &o;
   * RETURNS: None.
   */
  std::ostream &Print(std::ostream &o) override
  {
    for (auto el : DerWrtBias)
      o << el << ' ';
    return o;
  }
}; /* End of 'layer_bias' class */

/* Neural network max pool layer class */
class layer_pool : public layer
{
private:
  int S; // pool parameter

public:
  /* Class constructor.
   * ARGUMENTS:
   *  - bias for each depth:
   *      std::vector<double> bias.
   */
  layer_pool(int S) : S(S) {} /* End of 'layer_pool' function */

  /* Count result implementation function.
   * ARGUMENTS: None.
   * RETURNS: None.
   */
  void ForwardImpl() override
  {
    int InputH = (int)Input.GetSize(1);
    int InputW = (int)Input.GetSize(2);

    if (InputH % S != 0 || InputW % S != 0)
      throw "up";

    Output =
        array_t({Input.GetSize(0), (InputH + S - 1) / S, (InputW + S - 1) / S});
    for (int z = 0; z < Output.GetSize(0); z++)
      for (int y = 0; y < Output.GetSize(1); y++)
        for (int x = 0; x < Output.GetSize(2); x++)
        {
          int toplefty = y * S, topleftx = x * S;

          double max = -std::numeric_limits<double>::infinity();
          for (int i = 0; i < S; i++)
            for (int j = 0; j < S; j++)
            {
              double val = toplefty + i < Input.GetSize(1) && topleftx + j < Input.GetSize(2)
                               ? Input(z, toplefty + i, topleftx + j)
                               : -std::numeric_limits<double>::infinity();
              max = std::max(max, val);
            }
          Output(z, y, x) = max;
        }
  } /* End of 'ForwardImpl' function */

  /* Backward pass & count derivatives function.
   * ARGUMENTS:
   *  - [inout] derivative of cost function wrt to next layer input (this layer output):
   *      array_t &DerWrtInput;
   * RETURNS: None.
   */
  void Backward(array_t &DerWrtInput) override
  {
    // Count derivatives wrt input & kernels
    array_t der_wrt_input({Input.GetSize(0), Input.GetSize(1), Input.GetSize(2)});

    auto constexpr to_downscaled = [](int S, int Size, int Coord) {
      return Coord / S;
    };

    for (int z = 0; z < Input.GetSize(0); z++)
      for (int y = 0; y < Input.GetSize(1); y++)
        for (int x = 0; x < Input.GetSize(2); x++)
          if (int oy = to_downscaled(S, Input.GetSize(1), y),
              ox = to_downscaled(S, Input.GetSize(2), x);
              Output(z, oy, ox) == Input(z, y, x))
            der_wrt_input(z, y, x) = DerWrtInput(z, oy, ox);
    DerWrtInput.swap(der_wrt_input);
  } /* End of 'Backward' function */
}; /* End of 'layer_pool' class */

/* Neural network relu layer class */
class layer_relu : public layer
{
private:
  double alpha; // relu parameter

public:
  /* Class constructor.
   * ARGUMENTS:
   *  - bias for each depth:
   *      std::vector<double> bias.
   */
  layer_relu(double alpha) : alpha(alpha) {} /* End of 'layer_relu' function */

  /* Count result implementation function.
   * ARGUMENTS: None.
   * RETURNS: None.
   */
  void ForwardImpl() override
  {
    Output = array_t({Input.GetSize(0), Input.GetSize(1), Input.GetSize(2)});
    for (int z = 0; z < Output.GetSize(0); z++)
      for (int y = 0; y < Output.GetSize(1); y++)
        for (int x = 0; x < Output.GetSize(2); x++)
        {
          double val = Input(z, y, x);
          Output(z, y, x) = val < 0 ? val * alpha : val;
        }
  } /* End of 'ForwardImpl' function */

  /* Backward pass & count derivatives function.
   * ARGUMENTS:
   *  - [inout] derivative of cost function wrt to next layer input (this layer output):
   *      array_t &DerWrtInput;
   * RETURNS: None.
   */
  void Backward(array_t &DerWrtInput) override
  {
    for (int z = 0; z < Input.GetSize(0); z++)
      for (int y = 0; y < Input.GetSize(1); y++)
        for (int x = 0; x < Input.GetSize(2); x++)
          if (Input(z, y, x) < 0)
            DerWrtInput(z, y, x) *= alpha;
  } /* End of 'Backward' function */
};  /* End of 'layer_relu' class */

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(NULL);

  auto read = []<typename T>(std::type_identity<T>) {
    T i;
    std::cin >> i;
    return i;
  };
  auto read_int = [&read]() { return read(std::type_identity<int>{}); };
  auto read_dep = [&read_int] { return read_int() - 1; };
  auto do_read_vector = []<typename T>(std::type_identity<T>, int n, int delta = 0) {
    std::vector<T> vals(n);
    for (auto &val : vals)
    {
      std::cin >> val;
      val += delta;
    }
    return vals;
  };
  auto read_vector = [&do_read_vector, &read_int]<typename T>(std::type_identity<T> i,
                                                              int delta = 0) {
    return do_read_vector(i, read_int(), delta);
  };

  int wh = read_int(), d = read_int();
  auto output = array_t({d, wh, wh},
                        do_read_vector(std::type_identity<double>{}, wh * wh * d));

  int noof_layers = read_int();
  std::vector<std::unique_ptr<layer>> layers(noof_layers);
  int last_depth = d;
  for (auto &l : layers)
  {
    std::string type;
    std::cin >> type;

    if (type == "relu")
      l = std::make_unique<layer_relu>(1.0 / read_int());
    else if (type == "pool")
      l = std::make_unique<layer_pool>(read_int());
    else if (type == "bias")
      l = std::make_unique<layer_bias>(do_read_vector(std::type_identity<double>{}, last_depth));
    else /* if (type.starts_with("cnv")) */
    {
      int h = read_int();
      int k = read_int();
      int s = read_int();
      int p = read_int();
      array<std::vector<double>, 4> a(
          {h, last_depth, k, k},
          do_read_vector(std::type_identity<double>{}, h * last_depth * k * k));

      if (type.ends_with("m"))
        l = std::make_unique<layer_conv<padding::mirror>>(s, s, p, a);
      else if (type.ends_with("e"))
        l = std::make_unique<layer_conv<padding::extend>>(s, s, p, a);
      else /* if (type.ends_with("c")) */
        l = std::make_unique<layer_conv<padding::cyclic>>(s, s, p, a);

      last_depth = h;
    }

    array_t back_output;
    l->Forward(output, back_output);
    output.swap(back_output);
  }

  array_t der_wrt_input({output.GetSize(0), output.GetSize(1), output.GetSize(2)},
                        do_read_vector(std::type_identity<double>{}, output.GetSize()));

  for (auto li = layers.rbegin(); li != layers.rend(); ++li)
  {
    auto &l = *li;
    l->Backward(der_wrt_input);
  }

  for (auto el : output.GetVector())
    std::cout << el << ' ';
  std::cout << '\n';

  for (auto el : der_wrt_input.GetVector())
    std::cout << el << ' ';
  std::cout << '\n';

  for (auto &l : layers)
  {
    l->Print(std::cout);
    std::cout << '\n';
  }

  std::cout.flush();
  return 0;
}
