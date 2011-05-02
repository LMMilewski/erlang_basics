%%% This is based on eqc/examples/lists_eqc.erl

%% you must run this or you'll get a lot of compilation errors
%% code:add_patha("eqc/ebin/").

-module(eqc_simple).
-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").



%% test lists:reverse function
%% we check that reverse(reverse(X)) is identity
prop_reverse() ->
    ?FORALL(Xs, list(int()),
            lists:reverse(lists:reverse(Xs)) == Xs).
basic() ->
    quickcheck(prop_reverse()).

%% this will fail because lists:delete removes only first occurance of
%% element
prop_delete_fail() ->
    ?FORALL({X, Xs}, {int(), list(int())},
            not lists:member(X, lists:delete(X, Xs))).

prop_delete_fail_2() ->
    ?FORALL(Xs, non_empty(list(int())),
            ?FORALL(X, elements(Xs),
                    not lists:member(X, lists:delete(X, Xs)))).

delete_fail() ->
    quickcheck(numtests(10000, prop_delete_fail())).

delete_fail_2() ->
    quickcheck(prop_delete_fail_2()).


%% Note: you can then use counterexample to get data that recently
%% caused test to fail
%% type counterexample() in shell (you can later pass counterexample to check)
%%
%% You could also use backtrace(). This only works if the error
%% was not in another linked module


%% but if we ensure that the element occures no more than 1 time then
%% everything is ok
prop_delete_ok() ->
    ?FORALL({X, Xs}, {int(), list(int())},
            ?IMPLIES(length(lists:filter(fun(E) -> E == X end, Xs)) < 2,
                     not lists:member(X, lists:delete(X, Xs)))).
prop_delete_ok_2() ->
    ?FORALL({X, Xs, Ys}, {int(), list(int()), list(int())},
            ?IMPLIES(not lists:member(X, Xs),
                     equals(lists:delete(X, Xs ++ [X] ++Ys), % equals is true if arguments are equal
                            Xs ++ Ys))).

delete_ok() ->
    quickcheck(numtests(10000, prop_delete_ok_2())).


%% use collect function to check properties of the problem (for exampe
%% distribution). Here we check how often X is an element of a given
%% list
check_member_probability() ->
    quickcheck(?FORALL({X,Xs}, {int(), list(int())},
                       collect(with_title(member), lists:member(X, Xs),
                               collect(with_title(length), length(Xs),
                                       true)))).
%% OK, passed 100 tests
%% 96% false
%% 4% true
%% true

%% to make the probability higher lets imply that X is member of Xs
%% now it will fail almost always with only 100 tries
prop_delete_fail_fast() ->
    ?FORALL({X, Xs}, {int(), list(int())},
            ?IMPLIES(lists:member(X, Xs),
                     not lists:member(X, lists:delete(X, Xs)))).
delete_fail_fast() ->
    quickcheck(prop_delete_fail_fast()).


%% let's document failing case by creating property that uses eqc:fails
%% eqc:fails expectes at least one failing case
prop_fails_delete() ->
    fails(?FORALL({X, Xs}, {int(), list(int())},
                  ?IMPLIES(lists:member(X, Xs),
                           not lists:member(X, lists:delete(X, Xs))))).

%%% for more see other eqc examples in eqc/example/ diretory
%% make sure to see eqc/example/lists_eqc.erl
%% look for prop_seq part


%%% Another goot tutorials
%% http://www.protest-project.eu/upload/tutorials/TestingToolsWs/Software_Testing_with_QuickCheck1of3.pdf
%% http://www.protest-project.eu/upload/tutorials/TestingToolsWs/Software_Testing_with_QuickCheck2of3.pdf
%% http://www.protest-project.eu/upload/tutorials/TestingToolsWs/Software_Testing_with_QuickCheck3of3.pdf
