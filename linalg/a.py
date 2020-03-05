import numpy
import sympy
from numpy import linalg as LA
from sympy.solvers.solveset import linsolve
from sympy.interactive.printing import init_printing
init_printing(use_unicode=True)

a = sympy.Matrix(numpy.matrix(input()))
print('got:\n')
print(a)

print('characteristic poly: ')
print(a.charpoly().as_expr())

list = a.eigenvects()

for data in list:
  print('eigenvalue & power: ')
  print((data[0], data[1]))
  #print('eigenvector: ')
  #print(data[2])
  print()

dims = a.rows
sum = 0
basis = sympy.zeros(4, 0)
for l in list:
  b = a.copy()
  for i in range(0, dims):
    b[i, i] -= l[0]
  print(b)
  print('eigenspace for ' + str(l[0]))
  set = linsolve((b, sympy.zeros(dims, 1)))
  print(set)
  for v in l[2]:
    print(v)
    basis = basis.row_join(v)
    sum += 1

if (sum == dims):
  print('ops')
  print(basis)
  print(basis.inv() * a * basis)
  
