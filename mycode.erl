-module(mycode).
-export([start/0]).

%% erl -mode embedded (all code is loaded at startup)
%% erl -mode interactive (most code is loaded when first used)

%% erts refuses reloading kernel, stdlib, compiler directories (sticky dirs)

%% that's why you can't name module supervisor, code etc. (you'll get
%% "Can't load module that resides in sticky dir" error)
%% (supervisor.erl is in lib/stdlib/src/supervisor.erl)

%%% path manipulation
%%
%% set_path, get_path, add_path, add_pathz, add_patha, del_path, replace_path
start() ->
    %% mytest:start(), % ** exception error: undefined function mytest:start/0
    io:format("old path: ~p~n", [code:get_path()]),
    code:add_path("./code_dir/"),
    io:format("new path: ~p~n", [code:get_path()]),

    %% this is note necessery because mytest would be loaded with
    %% first use in interactive mode. In embedded mode the code will
    %% crash without following 2 lines
    code:add_path("./"),
    code:load_file(mytest),
    %% use module (in interactive mode would be loaded automatically,
    %% but not in embedded)
    mytest:start(),
    ok.

