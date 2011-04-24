-module(shell_cmds).

%%% Common shell pitfalls
%%
%% Using bound variables
%%  > X = 1.
%%  ...
%%  > X = 2. % no matching right hand side value
%%  > f(X). % or f().
%%  > X = 2. % now ok
%%
%% Creating fun with receive clause
%%  > spawn(fun() -> receive X -> X end). IS INCORRECT
%%  > spawn(fun() -> receive X -> X end end). IS OK
%%
%% Trying to use records
%%  > -record(person, {name,age}). % undefined shell command record
%%  > rd(person, {name, age}). % this is ok
%%

%%% Most common shell cmds
%% f() - forget all bindings (very common)
%% c(Module) - compile and load module
%% q() - quit

%%% Other very common shell cmds
%% help() -- print all available cmds
%% f(X) - forget X variable binding
%% v(N) - use value of query N
%% rd, rf, rl, rp, rr - record related cmds
%% bt(Pid) - backtrace (obvious)
%% flush() -> print all messages to the shell process
%% i() - print info about running processes
%% i(X,Y,Z) - print info about pid <<X.Y.Z>>
%% regs() - information about registered processes

%%% Cmds similar to linux cmds
%% cd, pwd, ls
