%% This file is meant to be an example of how to write tests in EUnit
%% for more informations how to test code with EUnit got to
%%
%% http://erlcode.wordpress.com/2010/08/30/erlang-eunit-introduction/
%% http://erlcode.wordpress.com/2010/08/30/erlang-eunit-continuation-1-fixtures/
%% http://erlcode.wordpress.com/2010/08/30/erlang-eunit-continuation-2-test-representation/
%% http://erlcode.wordpress.com/2010/09/01/erlang-eunit-continuation-3-test-control/
%% http://erlcode.wordpress.com/2010/10/10/eunit-common-special-errors/

-module(fib_tests).
-include_lib("eunit/include/eunit.hrl").

negative_argument_test() ->
    ?assertException(error, function_clause, fib:compute(-1)),
    ?assertException(error, function_clause, fib:compute(-100)).

small_argument_test() ->
    ?assertEqual(0, fib:compute(0)),
    ?assertEqual(1, fib:compute(1)),
    ?assertEqual(1, fib:compute(2)),
    ?assertEqual(2, fib:compute(3)),
    ?assertEqual(3, fib:compute(4)),
    ?assertEqual(5, fib:compute(5)).

big_argument_test() ->
    ?assertEqual(4181, fib:compute(19)),
    ?assertEqual(1134903170, fib:compute(45)),
    ?assertEqual(7778742049, fib:compute(49)).

