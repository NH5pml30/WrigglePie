import numpy
import sympy
import scipy.signal
from numpy import linalg as LA
from sympy.solvers.solveset import linsolve
from sympy.interactive.printing import init_printing
init_printing(use_unicode=True)
t = sympy.symbols("t")

def poly_decompose(phi: sympy.Poly):
  roots = sympy.polys.polyroots.roots(phi)
  poly = sympy.Poly(0, t)
  roots_sym = []
  j = 0
  for root, pow in roots.items():
    for i in range(1, pow + 1):
      mult = sympy.div(phi, (t - root) ** i)[0]
      symbol = sympy.symbols('x' + str(j))
      j += 1
      roots_sym.append(symbol)
      poly += mult * symbol

  coeffs = poly.all_coeffs()
  matr_list = []
  i = 0
  j = 0
  for coef in coeffs:
    matr_row_list = []
    for symbol in roots_sym:
      all_coeffs = coef.as_poly(symbol).all_coeffs()
      if (len(all_coeffs) == 2):
        matr_row_list.append(all_coeffs[0])
      else:
        matr_row_list.append(0)
    matr_list.append(matr_row_list)
  matr = sympy.Matrix(matr_list)

  b = sympy.zeros(matr.rows, 1)
  b[-1] = 1
  set = linsolve((matr, b))

  assert len(set.args) == 1
  ans = set.args[0]

  j = 0
  res = dict()
  for root, pow in roots.items():
    poly = sympy.Poly(0, t)
    for i in range(1, pow + 1):
      poly += ans[j] * ((t - root) ** (pow - i))
      j += 1
    res[root] = poly
  return res

def get_projectors(a):
  print('characteristic poly: ')
  print(a.charpoly().factor())
  print('\n')

  n = a.rows

  min_poly = sympy.Poly(1, t)

  for i in range(n):
    e = sympy.zeros(n, 1)
    e[i] = 1;
    print('\n--- for ', end = '')
    print(e, end = '')
    print(' ---')
    matr = sympy.zeros(n, 0)
    b = sympy.zeros(n, 1)
    while True:
      b = e
      newmatr = matr.row_join(e)
      if newmatr.rank() != newmatr.cols:
        break
      e = a * e
      matr = newmatr
    print(matr)
    print(b)
    x = sympy.Matrix(linsolve((matr, b)).args[0])
    poly = sympy.Poly(t ** matr.cols, t)
    for i in range(matr.cols):
      poly += t ** i * -x[i]
    print(poly)
    print(poly.factor())
    min_poly = sympy.lcm(min_poly, poly)
    print()

  print()
  print('minimal poly: ')
  print(min_poly)
  print(min_poly.factor())

  roots = sympy.polys.polyroots.roots(min_poly)
  print(roots)

  list = a.eigenvects()

  for l in list:
    print()
    print('eigenvalue: ', end='')
    print(l[0])
    print('multiplicity: ', end='')
    print(l[1])
    print('minimal multiplicity: ', end='')
    print(roots[l[0]])
    print()

    b = a.copy()
    for i in range(0, n):
      b[i, i] -= l[0]
    bpow = b ** roots[l[0]]
    print('A-lambdaE: ', end='')
    print(b)
    print('eigenspace for ' + str(l[0]))
    set = linsolve((b, sympy.zeros(n, 1)))
    print(set)
    for v in l[2]:
      print(v)
    print('(A-lambdaE)^m(lamda): ', end='')
    print(bpow)
    print('root subspace for ' + str(l[0]))
    set = linsolve((bpow, sympy.zeros(n, 1)))
    print(set)
    for v in l[2]:
      print(v)
    print()

  pp = sympy.Poly(1, t)
  i = 0
  symbols = []
  for l in list:
    fli = sympy.Symbol('fl_' + str(i))
    symbols.append(fli)
    pp -= fli * sympy.div(min_poly, (t - l[0]) ** roots[l[0]])[0]
    i += 1

  oneM = sympy.zeros(n, n);
  one = sympy.Poly(1, t)
  f = poly_decompose(min_poly)

  res_list = []

  for l in list:
    f_li = f[l[0]]
    print('got f_' + str(l[0]) + ': ', end='')
    print(f_li)
    p_li = (f_li * sympy.div(min_poly, (t - l[0]) ** roots[l[0]])[0]).as_poly()
    print('p_' + str(l[0]) + ': ', end = '')
    print(p_li)
    print(p_li.all_coeffs())
    try:
      print(p_li.factor())
    except:
      print('No factors')
    coefs = p_li.all_coeffs()
    coefs.reverse()
    P_li = sympy.zeros(n, n)
    for i in range(len(coefs)):
      P_li += a ** i * coefs[i]
    print('P_' + str(l[0]) + ': ', end = '')
    print(P_li)
    print(P_li.columnspace())
    oneM += P_li
    one += p_li
    res_list.append(((l[0], l[1], roots[l[0]]), P_li));
  print(one)
  print(oneM)

  return res_list

def find_kernel(a):
  a = sympy.Matrix(a)
  solved = linsolve((a, sympy.zeros(a.rows, 1)))
  args = solved.args
  res = []
  for symbol in solved.free_symbols:
    values = dict()
    for s in solved.free_symbols:
      if symbol != s:
        values[s] = 0
      else:
        values[s] = 1
    v = sympy.zeros(0, 1)
    for arg in args[0]:
      v = v.col_join(sympy.Matrix([[arg.as_poly(solved.free_symbols).eval(values)]]))
    res.append(v)
  return res

def check_linear_independence(vs):
  matr = vs[0]
  for i in range(1, len(vs)):
    matr = matr.row_join(vs[i])
  return matr.rank() == len(vs)
def throw_out_dependent_vector(vs, v):
  v = sympy.Matrix(v)
  vvs = vs.copy()
  vvs.append(v)
  if check_linear_independence(vvs):
    return vs;
  for i in range(len(vs)):
    list = vvs.copy()
    del list[i]
    if check_linear_independence(list):
      list.pop()
      return list
  assert False
def choose_independent(vs1, vs2):
  for v in vs2:
    if (check_linear_independence(vs1 + [v])):
      return v
  assert False
def throw_out_dependent_vectors(vs_list, v):
  return [throw_out_dependent_vector(vs, v) for vs in vs_list]
def independent_subsystem(vs):
  i = 0
  while i < len(vs):
    if not check_linear_independence(vs[0:i+1]):
      del vs[i]
    else:
      i += 1
  return vs

a = sympy.Matrix(numpy.matrix(input()))
print('got:\n')
print(a)

n = a.rows

proj = get_projectors(a)

D = sympy.zeros(n, n)
m = 0
for l in proj:
  D += l[0][0] * l[1]
  m = max(m, l[0][2])
B = a - D
print('D: ')
print(D)
print('B: ')
print(B)
print('B^nu = B^' + str(m) + ':')
print(B ** m)
for i in range(m):
  z = B ** i
  assert z != sympy.zeros(n, n)
print('less powers are not zero')

T = sympy.zeros(n, 0)

for li in proj:
  l = li[0]
  k = []
  print('\nBegin search for Jordan basis for ' + str(l[0]) + '...')
  for i in range(l[2]):
    matr = sympy.Matrix((a - l[0] * sympy.Identity(n)) ** (i+1))
    print('Solve for kernel of matrix: ')
    print(matr)
    k.append(find_kernel(matr))
    print('K' + str(i + 1) + ' basis vectors: ')
    for v in k[-1]:
      print(v, end=' ')
    print()
  j = []
  shallow_j = []
  for i in range(l[2] - 1, 0, -1):
    while True:
      jmax = sympy.zeros(n, 1)
      for maybe_j in k[i]:
        if check_linear_independence(independent_subsystem(k[i - 1] + shallow_j) + [maybe_j]):
          jmax = maybe_j
          break
      if jmax == sympy.zeros(n, 1):
        print('no available basis vectors in K' + str(i + 1) + ' left...');
        break

      print('choose new basis vector for K' + str(i + 1) + ': ', end='');
      print(jmax);
      j.insert(0, [jmax])
      # k = throw_out_dependent_vectors(k, jmax)
      for m in range(i):
        print('project down basis vector to K' + str(i - m) + ': ', end='');
        jmax = sympy.Matrix((a - l[0] * sympy.Identity(n)) * jmax)
        print(jmax)
        j[0].append(jmax)
        # k = throw_out_dependent_vectors(k, jmax)
      reversed_list = j[0].copy()
      reversed_list.reverse()
      shallow_j = reversed_list + shallow_j

  print('Only K1 left, dumping its basis vectors to result:');
  for jj in k[0]:
    if check_linear_independence(shallow_j + [jj]):
      j.insert(0, [jj])
      shallow_j.insert(0, jj)
      print(jj)
  print('Jordan basis for ' + str(l[0]) + ':');
  for jj in shallow_j:
    print(jj)
    T = T.row_join(jj)
  print()

print('Got T matrix: ')
print(T)
print('Calculated J from T and A: ')
print((T ** -1) * a * T)
