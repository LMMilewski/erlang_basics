-module(myauth).
-export([what_is_my_cookie/0, set_my_cookie/0]).

%% When you have two nodes and can't make them communicate you
%% probably forgot to set cookies

%% Two nodes can communicate only if both have the same cookie. (~/.erlang.cookie file)

%% To check that you can communicate with another node type:
%%  > auth:is_auth(AnotherNode).
%% if it returns yes, you can set it to no by changeing cookie

what_is_my_cookie() ->
    erlang:get_cookie(). % returns node()'s cookie. auth:cookie/0 is deprecated

set_my_cookie() ->
    erlang:set_cookie(node(), 'hello world'), % this sets own cookie to atom 'hello world'
    erlang:get_cookie().

