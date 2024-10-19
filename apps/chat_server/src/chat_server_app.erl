%%%-------------------------------------------------------------------
%% @doc chat_server public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).

-behaviour(application).

-export([start/2, stop/1]).


-define(PORT, 4000).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).


start(_StartType, _StartArgs) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    io:format("Listening on port ~p~n", [?PORT]),
    accept(ListenSocket).

stop(_State) ->
    ok.

%% internal functions

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("New client connected~n"),
    spawn(fun() -> loop(Socket) end),
    accept(ListenSocket). % continue accepting new connections

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Received: ~p~n", [Data]),
            gen_tcp:send(Socket, Data), % echo data back to client
            loop(Socket); % continue handling messages
        {error, closed} ->
            io:format("Client disconnected~n"),
            gen_tcp:close(Socket)
        end.
