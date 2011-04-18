%% http://www.erlang.org/doc/design_principles/events.html
%% http://www.erlang.org/doc/man/gen_event.htlm
%% http://www.trapexit.org/Gen_event_behavior_demystified
-module(own_event_manager).

%% API
-export([start/0, start_link/0, add_handler/1, msg/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================
start() ->
    gen_event:start({local, ?SERVER}).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Module) ->
    io:format ("manager add_handler ~p~n", [Module]),
    gen_event:add_handler(?SERVER, Module, []).

msg(Msg) ->
    gen_event:notify(?SERVER, Msg).
