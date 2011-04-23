-module(myrpc).
-export([say_hello/1, reload/1]).

%%%
%% rpc runs function on remote node (or nodes) and collets the result

%% Run two nodes a and b.

%% On b call (replace HOST with your hostname)
%%  > rpc:call(a@HOST, myrpc, say_hello, [node()]).     % or rpc:cast with the same parameters
%% You should get
%% Say hello b@HOST on node a@HOST
say_hello(Caller) ->
    io:format("Hello ~p on node ~p~n", [Caller, node()]).

%% Reloads myrpc module on remote node (given as a parameter)
%% If you run this funciton on node b
%%  > myrpc:reaload(a@HOST).
%% then you will reload the code on node a@HOST (and not on b@HOST).
reload(Node) ->
    rpc:call(Node, code, soft_purge, [myrpc]), % soft_purge removes the code if it is not used by any process (purge would kill all processes that use the code)
    rpc:call(Node, compile, file, [myrpc]), % this recompiles the module
    rpc:call(Node, code, load_file, [myrpc]).


%% instead of call you can also use
%%  block_call
%% which will block rpc
%% service while the call is being processed or
%%  async_call / yield
%% if the caller should not blokc

%% there is also multicall function family used to call functions on
%% many nodes


