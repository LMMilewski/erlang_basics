-module(remotecodeload).
-export([send/0, start/0]).

%% This may sound crazy but you can send the module code over the
%% network to another node, load it there and call it
%%
%% In this example we send remotecodeload module to all nodes in
%% Erlang cloud.
%%
%% Go to directory with remotecodeload (it should not be /tmp/) module and run
%%  $ erl -sname a
%% Now go to /tmp/ in 3 other terminals and run
%%  $ erl -sname b
%%  $ erl -sname c
%%  $ erl -sname d
%%
%% Assuming that HOST is your hostname
%% on 'a' node run command
%%  > lists:foreach(fun net_adm:ping/1, [a@lmm, b@lmm, c@lmm, d@lmm]).  % this will connect all nodes in cloud
%%  > remotecodeload:start().
%%
%% on 'b', 'c', 'd' nodes run
%%  > remotecodeload:start() % you should get error saying that this module does not exist
%%
%% go back to 'a' node and call
%%  > remotecodeload:send()
%%
%% last command sends the module to all nodes and run
%% remotecodeload:start() on each. Now you can go to any node and
%% check that
%%  > remotecodeload:start
%% will work (previously Erlang complained that the module does not exist)

start() ->
    %% this will print the hello messages and information on which
    %% node it was run
    io:format("hello world. I live on node ~p~n", [node()]).

send() ->
    {Mod, Bin, File} = code:get_object_code(remotecodeload), % get the binary code of our module
    io:format("Sending code to nodes: ~p~n", [[node() | nodes()]]), % print were we send it (all nodes in the cloud)
    rpc:multicall(code, load_binary, [Mod, File, Bin]), % load the binary on every node
    rpc:multicall(remotecodeload, start, []), % call remotecodeload:start() on all nodes
    ok.
