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


a = sympy.Matrix(numpy.matrix(input()))
print('got:\n')
print(a)

print('characteristic poly: ')
print(a.charpoly().factor())
print('\n')

n = a.rows

# print(sympy.Poly())

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

#for data in list:
#  print('eigenvalue & power: ')
#  print((data[0], data[1]))
#  #print('eigenvector: ')
#  #print(data[2])
#  print()

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
for l in list:
  #print('input f_' + str(l[0]) + ':', end = ' ')
  #lll = input().split(',')
  #lll = [sympy.simplify(i) for i in lll]
  #coefs = sympy.Matrix(lll)
  #f_li = sympy.Poly(coefs, t)
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
  # P_li = p_li.eval(a)
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
print(one)
print(oneM)

#if (sum == dims):
#  print('ops')
#  print(basis)
#  print(basis.inv() * a * basis)
  
