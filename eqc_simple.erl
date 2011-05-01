-module(eqc_simple).
%% -export([basic/0, delete_fail/0, delete_ok/0]).
-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").

%% you must run this or you'll get a lot of compilation errors
%% code:add_patha("eqc/ebin/").

%% test lists:reverse function
%% we check that reverse(reverse(X)) is identity
prop_reverse() ->
    ?FORALL(Xs, list(int()),
            lists:reverse(lists:reverse(Xs)) == Xs).
basic() ->
    eqc:quickcheck(prop_reverse()).

%% this will fail because lists:delete removes only first occurance of
%% element
prop_delete_fail() ->
    ?FORALL({X, Xs}, {int(), list(int())},
            not lists:member(X, lists:delete(X, Xs))).

delete_fail() ->
    eqc:quickcheck(eqc:numtests(10000, prop_delete_fail())).
%% Note: you can then use eqc:counterexample to get data that recently
%% caused test to fail
%% type eqc:counterexample() in shell (you can later pass counterexample to eqc:check)
%%
%% You could also use eqc:backtrace(). This only works if the error
%% was not in another linked module


%% but if we ensure that the element occures no more than 1 time then
%% everything is ok
prop_delete_ok() ->
    ?FORALL({X, Xs}, {int(), list(int())},
            ?IMPLIES(length(lists:filter(fun(E) -> E == X end, Xs)) < 2,
                     not lists:member(X, lists:delete(X, Xs)))).
delete_ok() ->
    eqc:quickcheck(eqc:numtests(10000, prop_delete_ok())).

