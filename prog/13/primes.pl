flag_composite(N, P) :- sito(N), !.
flag_composite(N, P) :-
  assert(sito(N)),
  assert(min_divisor(N, P)).

flag_composites(N, MAX) :- N2 is N * N, flag_composites(N2, N, MAX).
flag_composites(4, 2, MAX) :- !, flag_composites(4, 2, 2, MAX).
flag_composites(N2, N, MAX) :- sito(N), !.
flag_composites(N2, N, MAX) :- Nt is N + N, flag_composites(N2, Nt, N, MAX).
flag_composites(N, Stride, N0, MAX) :- N > MAX, !.
flag_composites(N, Stride, N0, MAX) :-
  flag_composite(N, N0),
  N1 is N + Stride,
  flag_composites(N1, Stride, N0, MAX).

init_odd(Min, MAX) :- Min2 is Min * Min, init_odd(Min2, Min, MAX).
init_odd(Min2, Min, MAX) :- Min2 > MAX, !.
init_odd(Min2, Min, MAX) :-
  flag_composites(Min2, Min, MAX),
  Next is Min + 2,
  Next2 is Min2 + 4 * Min + 4,
  init_odd(Next2, Next, MAX).
init(MAX) :- assert(last(MAX)), flag_composites(2, MAX), init_odd(3, MAX).

check_bounds(N) :- N > 1, last(MAX), N =< MAX.
prime(N) :- check_bounds(N), \+ sito(N).
composite(N) :- check_bounds(N), sito(N).

prime_product([], Min, X) :- X = 1, !.
prime_product([H | T], Min, X) :- H >= Min, prime(H),
  prime_product(T, H, O), X is H * O.
prime_divisors(1, []) :- !.
prime_divisors(N, D) :- var(N), !, prime_product(D, 2, N).
prime_divisors(N, D) :- prime(N), D = [N], !.
prime_divisors(N, [H | T]) :-
  min_divisor(N, H),
  Next is div(N, H),
  prime_divisors(Next, T).
