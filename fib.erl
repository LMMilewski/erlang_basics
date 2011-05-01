-module(fib).
-export([compute/1]).

compute(N) when N >= 0 ->
    compute(N, 0, 1).

compute(0, F1, _F2) ->
    F1;
compute(N, F1, F2) ->
    compute(N-1, F2, F1+F2).


