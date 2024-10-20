%%%-------------------------------------------------------------------
%% @doc chat server entrypoint
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).

-behaviour(application).

-export([start/2, stop/1, loop/1]).

-define(DEFAULT_PORT, 4000).

start(_StartType, _StartArgs) ->
    Port = application:get_env(chat_server, port, ?DEFAULT_PORT),
    socket_server_sup:start_link(?MODULE, Port,  {?MODULE, loop}).

stop(_State) ->
    ok.

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Received: ~p~n", [Data]),
            gen_tcp:send(Socket, Data),
            loop(Socket);
        {error, closed} ->
            io:format("Client disconnected~n"),
            ok
    end.
