%% To add your own logger you implement it as gen_event
%% You have to add register_logger function to the API
%% You can handle any type of logging event. see
%%  http://www.erlang.org/doc/man/error_logger.html#add_report_handler-1
%%  for list of possible events (section "Events" at the end).

-module(custom_logging).

-behaviour(gen_event). % we need to implement gen_event to plug our own logger

%% API
-export([start_link/0, add_handler/0, register_logger/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%% we add this function to the API
register_logger() ->
    error_logger:add_report_handler(?MODULE). % register this module as logger

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

% our own error handler for error_logger:error_msg function
handle_event({error, _Gleader, {Pid, Format, Data}}, State) ->
    io:format("ERROR <~p> ~s~n", [Pid, io_lib:format(Format, Data)]),
    {ok, State};
handle_event(_Event, State) -> % discard all other events
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
