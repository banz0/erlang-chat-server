%%%-------------------------------------------------------------------
%% @doc chat_server actual server implementation.
%% @end
%%%-------------------------------------------------------------------

-module(socket_server).

-behavior(gen_server).

-export([
    start/3,
    init/1,
    handle_cast/2,
    accept_loop/1,
    accept/1,
    handle_call/3,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
	port,
	loop,
	ip=any,
	listen_socket=null
}).

start(Name, Port, Loop) ->
	State = #server_state{port = Port, loop = Loop},
	gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
   		{ok, ListenSocket} ->
            io:format("Listening on port ~p~n", [Port]),
   			NewState = State#server_state{listen_socket = ListenSocket},
   			{ok, accept(NewState)};
   		{error, Reason} ->
   			{stop, Reason}
	end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
	{noreply, accept(State)}.

accept_loop({Server, ListenSocket, {M, F}}) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("New client connected~n"),
	gen_server:cast(Server, {accepted, self()}),
	M:F(Socket).

%% internal functions

accept(State = #server_state{listen_socket=ListenSocket, loop = Loop}) ->
	proc_lib:spawn(?MODULE, accept_loop, [{self(), ListenSocket, Loop}]),
	State.

%% dummy implementations to suppress warnings

handle_call(_Request, _From, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
