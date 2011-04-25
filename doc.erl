-module(doc).
-export([sum/2, example/0]).

%%% Conventions

%% In Erlang there are three types of comments in use.
%%
%% First one starts with three % chars (%%%). These are comments about the
%% module. 
%% Second is %% (two % chars) - comments about the function.
%% Last one is single % character - about Erlang code. 

%%% API functions - module level comment
example() ->
	  %% prints some text - function level comment
	  io:format("hello~n"). % statement level comment

%% @doc Short descriptio
%%
%% Then empty line and longer description.
%%
%% To specify type of the function you use -spec directive
-spec sum(integer(), integer()) -> integer(). % notice the dot at the end
sum(A, B) ->
     A + B.

%%% Type specyfications
%% for list of possible types see
%% http://www.erlang.org/doc/reference_manual/typespec.html
