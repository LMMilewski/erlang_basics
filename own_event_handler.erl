%% http://www.erlang.org/doc/design_principles/events.html
%% http://www.erlang.org/doc/man/gen_event.htlm
%% http://www.trapexit.org/Gen_event_behavior_demystified

-module(own_event_handler).
-behaviour(gen_event).
-export([init/1, add_handler/0, delete_handler/0, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% this function is called when the handler is added to the event manager
%% arguments are arguments passed to gen_event:add_handler/2
init(Args) ->
    io:format("handler init ~p~n", [Args]),
    {ok, []}.

%% adds this handler to own_event_manager event manager
add_handler() ->
    gen_event:add_handler(own_event_manager, ?MODULE, []).

%% deletes this handler from own_event_manager, event manager
delete_handler() ->
    gen_event:delete_handler(own_event_manager, ?MODULE, []).

%% thsi is called when the function gen_event:notify/1 is called - see
%% own_event_manager)
handle_event(Event, State) ->
    io:format("handler Event ~p~n", [Event]),
    {ok, State}.

%% this is called when gen_event:call is called. This like in
%% gen_server
handle_call(Request, State) ->
    io:format("handler handle_call ~p~n", [Request]),
    {ok, ok, State}.

%% called when there is a message from outside gen_event behaviour
handle_info(Info, State) ->
    io:format("handler handle_info ~p~n", [Info]),
    {ok, State}.

%% called when the handler is removed with gen_event:delete_handler/2
terminate(Arg, _State) ->
    io:format("handler terminate ~p~n", [Arg]),
    ok.

%% called whenever there is code upgrade/downgrade
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
