-module(logging).
-export([run/0, spawn_sasl/0, spawn_nosasl/0]).
-export([die/0]).

-define(LOGFILE, "/tmp/logs").

%% Remember to start erl like this
%% erl +W w
%% this will make warnings look like warnings instead of errors

run() ->
    %% first start System Application Support Libraries
    %% this will give you *much* more error info when something crashes
    application:start(sasl),

    %% the format of all functions is like io:format
    %% i.e.
    %% error_logger:info_msg(Message)
    %% error_logger:info_msg(FormatString, ListOfParameters)
    %% This is pretty much like printf in C, but with different letters. Most important:
    %%   ~p   - pretty print argument
    %%   ~w   - raw print argument
    %%   ~n   - newline
    error_logger:info_msg("SIMPLE info"), % you can print a string
    error_logger:info_msg("INFO with parameter ~p", [{complex_tuple, [1,2,3,4,5]}]),
    error_logger:warning_msg("simple WARNING"),
    error_logger:warning_msg("WARNING with parameter ~p", [{complex_tuple, [1,2,3,4,5]}]),
    error_logger:error_msg("simple ERROR"),
    error_logger:error_msg("ERROR with parameter ~p", [{complex_tuple, [1,2,3,4,5]}]),

    error_logger:info_msg("~p", [[65,66,67]]), % this will print "ABC" ;-/
    error_logger:info_msg("~w", [[65,66,67]]), % this does the job - print [65,66,67] ;-)


    %% you can have only one logfile opened, so make sure you don't
    %% have opened log devices
    error_logger:logfile(close), % this will return error if no logfile is opened

    %% let's write logs to file
    ok = error_logger:logfile({open, ?LOGFILE}), % the file is created or truncated
    {error, allready_have_logfile} = error_logger:logfile({open, "/tmp/second_logs"}), % only one logfile allowed
    error_logger:info_msg("this message will be printed to the tty and to the file"),
    error_logger:tty(false), % turn off tty logging
    error_logger:info_msg("this message will be printed ONLY to the file"),
    error_logger:logfile(close),
    error_logger:info_msg("this message won't be printed"),
    error_logger:tty(true), % turn on tty logging
    error_logger:info_msg("this message will be printed only to the tty").


%% this will give us a lot of information when function 'die' crashes
spawn_sasl() ->
    application:start(sasl),
    proc_lib:spawn(fun die/0). % this gives us a lot of error report text

%% this won't give us any useful information when function 'die' crashes
spawn_nosasl() ->
    application:stop(sasl),
    proc_lib:spawn(fun die/0). % this gives us a lot of error report text

die() ->
    this_wont = match.
