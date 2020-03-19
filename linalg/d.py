import numpy
import sympy
import scipy.signal
from numpy import linalg as LA
from sympy.solvers.solveset import linsolve
from sympy.interactive.printing import init_printing
init_printing(use_unicode=True)
t = sympy.symbols("t")

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
def matrix2vecs(m):
  return [m.col(i) for i in range(m.cols)]
def vecs2matrix(vs):
  res = sympy.zeros(vs[0].rows, 0)
  for v in vs:
    res = res.row_join(v)
  return res

a = sympy.Matrix(numpy.matrix(input()))
print('got:\n')
print(a)

print('characteristic poly: ')
print(a.charpoly().factor())
print('\n')
n = a.rows
charpoly = a.charpoly()

eigenvals = charpoly.real_roots()

print(charpoly.real_roots(multiple=False))

for li in charpoly.real_roots(multiple=False):
  l = li[0]
  print('Begin search for m(' + str(l) + ')')
  Bl = a - l * sympy.eye(n)
  K = find_kernel(Bl)
  m = 1
  while len(K) < eigenvals.count(l):
    m += 1
    print('check Ker(B**' + str(m) + ')')
    print(Bl ** m)
    K = find_kernel(Bl ** m)
    print('basis of kernel:')
    print(K)
  print('found m(lambda) = ' + str(m))
  print('found root subspace: ')
  print(K)
  print('BK: ')
  fixed = independent_subsystem(matrix2vecs(Bl * vecs2matrix(K)))
  print(fixed)
  basis = []
  size = len(K) - len(fixed)
  for i in range(size):
    vi = choose_independent(fixed, K)
    fixed.append(vi)
    print('v' + str(i) + ':')
    print(vi)
    print('Building cyclic basis on it')
    basis.append(vi)
    while True:
      vi = Bl * vi
      if vi != sympy.zeros(n, 1):
        print('New vector: ')
        print(vi)
        basis.append(vi)
      else:
        print('Got 0, stopping')
        break
