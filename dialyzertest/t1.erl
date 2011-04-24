-module(t1).
-export([test1/0, test2/0, test3/0).

test1() ->
	list:seq(10). % should be lists:seq

test2() ->
	t3:test1(). % module does not exist

test3() ->
	[1|2], % improper list
	ok.
