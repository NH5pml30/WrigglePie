import numpy
import sympy
import scipy.signal
from numpy import linalg as LA
from sympy.solvers.solveset import linsolve
from sympy.interactive.printing import init_printing
init_printing(use_unicode=True)

a = sympy.Matrix(numpy.matrix(input()))
print('got:\n')
print(a)

print('characteristic poly: ')
print(a.charpoly().as_expr())
print('\n')

n = a.rows
t = sympy.symbols("t")

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
  min_poly = sympy.lcm(min_poly, poly)
  print()

print()
print('minimal poly: ')
print(min_poly)

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

coefs = min_poly.all_coeffs()
res = scipy.signal.residue([1], coefs)
print(res)

one = sympy.zeros(n, n);
for l in list:
  print('input f_' + str(l[0]) + ':', end = ' ')
  lll = input().split(',')
  lll = [sympy.simplify(i) for i in lll]
  coefs = sympy.Matrix(lll)
  f_li = sympy.Poly(coefs, t)
  p_li = (f_li * sympy.div(min_poly, (t - l[0]) ** roots[l[0]])[0]).as_poly()
  print('p_' + str(l[0]) + ': ', end = '')
  print(p_li)
  # P_li = p_li.eval(a)
  coefs = p_li.all_coeffs()
  coefs.reverse()
  P_li = sympy.zeros(n, n)
  for i in range(len(coefs)):
    P_li += a ** i * coefs[i]
  print('P_' + str(l[0]) + ': ', end = '')
  print(P_li)
  one += P_li
print(one)

#if (sum == dims):
#  print('ops')
#  print(basis)
#  print(basis.inv() * a * basis)
  
