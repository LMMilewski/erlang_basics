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
%% Getting stuck in shell
%%  > receive X -> X end. % after this command the shell is block
%% type ^G
%% --> s
%% --> j
%%  you get list of all jobs
%% --> c N % N - id of new shell job (i.e. 2)
%% if you want to kill previous shell
%% type ^G
%% --> j
%% --> k N % N - id of the job you want to kill
%% 
%% Easier way:
%% type ^G
%% --> i
%% --> c
%%
%% This (both ways) kills the shell (it is obviously restarted). 
%% You lose your ETS tables + all linked processes are killed
%% If you can't affort loosing these you can switch (^G s c) 
%% to another shell and repair the situation (i.e. send a message
%% to process stuck in receive expression).


%%% Most common shell cmds
%% f() - forget all bindings (very common)
%% c(Module) - compile and load module
%% q() - quit

%%% ^G commands (type ctrl+g to run 'user switch commnad')
%% h - print all available commands

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
