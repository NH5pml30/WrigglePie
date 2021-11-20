% Review + delay

flag_composite(N, P) :- sito(N), !.
flag_composite(N, P) :-
  assert(sito(N)),
  assert(min_divisor(N, P)).

flag_composites(2, MAX) :- !, do_flag_composites(4, 2, 2, MAX).
flag_composites(N, MAX) :- sito(N), !.
flag_composites(N, MAX) :- N1 is N * N, Nd is N + N, do_flag_composites(N1, N, Nd, MAX).
do_flag_composites(N, N0, Stride, MAX) :- N > MAX, !.
do_flag_composites(N, N0, Stride, MAX) :-
  flag_composite(N, N0),
  N1 is N + Stride,
  do_flag_composites(N1, N0, Stride, MAX).

init_odd(Min, MAX) :- Min >= MAX, !.
init_odd(Min, MAX) :-
  flag_composites(Min, MAX),
  Next is Min + 2,
  init_odd(Next, MAX).
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

to_base_(0, K, Res) :- !, Res = [].
to_base_(N, K, Res) :-
  Lowest is mod(N, K),
  Left is div(N, K),
  to_base_(Left, K, Tail),
  Res = [Lowest | Tail].
to_base(0, K, Res) :- !, K > 1, Res = [0].
to_base(N, K, Res) :- K > 1, to_base_(N, K, Res).
palindrome(D) :- reverse(D, D).
prime_palindrome(N, K) :- prime(N), to_base(N, K, List), palindrome(List).
