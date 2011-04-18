%% http://www.erlang.org/doc/design_principles/events.html
%% http://www.erlang.org/doc/man/gen_event.htlm
%% http://www.trapexit.org/Gen_event_behavior_demystified

-module(own_event_handler).
-export([init/1, add_handler/0, delete_handler/0, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

init(Args) ->
    io:format("handler init ~p~n", [Args]),
    {ok, []}.

add_handler() ->
    gen_event:add_handler(own_event_manager, ?MODULE, []).

delete_handler() ->
    gen_event:delete_handler(own_event_manager, ?MODULE, []).

handle_event(Event, State) ->
    io:format("handler Event ~p~n", [Event]),
    {ok, State}.

handle_call(Request, State) ->
    io:format("handler handle_call ~p~n", [Request]),
    {ok, ok, State}.

handle_info(Info, State) ->
    io:format("handler handle_info ~p~n", [Info]),
    {ok, State}.

terminate(Arg, _State) ->
    io:format("handler terminate ~p~n", [Arg]),
    ok.
