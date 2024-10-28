%%%-------------------------------------------------------------------
%% @doc chat server entrypoint
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).

-behaviour(application).

-export([start/2, stop/1, pre_loop/1]).

-define(DEFAULT_PORT, 4000).

start(_StartType, _StartArgs) ->
    Port = application:get_env(chat_server, port, ?DEFAULT_PORT),
    chat_handler_sup:start_link(),
    socket_server_sup:start_link(?MODULE, Port,  {?MODULE, pre_loop}).

stop(_State) ->
    ok.

pre_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Received data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            {Command, [_|Nick]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message), % TODO check if list has more than one element first
            case Command of
                "CONNECT" ->
                    try_connection(clean(Nick), Socket);
                _ ->
                    gen_tcp:send(Socket, "Unknown command!\n"),
                    ok
            end;
        {error, closed} ->
            ok
    end.

try_connection(Nick, Socket) ->
    Response = gen_server:call(chat_handler, {connect, Nick, Socket}),
    case Response of
        {ok, List} ->
            gen_tcp:send(Socket, "CONNECT:OK:" ++ List ++ "\n"),
            gen_server:cast(chat_handler, {join, Nick}),
            loop(Nick, Socket);
        nick_in_use ->
            gen_tcp:send(Socket, "CONNECT:ERROR:Nick in use.\n"),
            ok
    end.

loop(Nick, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [Data]),
            Message = binary_to_list(Data),
            handle_message(Nick, Message, Socket);
        {error, closed} ->
            io:format("Client disconnected~n"),
            % TODO update users_list?
            ok
    end.

handle_message(Nick, Message, Socket) ->
    case lists:member($:, Message) of
        true ->
            {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            handle_command(Nick, Command, Content, Socket);
        false ->
            % ignore messages that do not follow COMMAND:content structure
            loop(Nick, Socket)
    end.

handle_command(Nick, Command, Content, Socket) ->
    case Command of
        "SAY" ->
            say(Nick, Socket, clean(Content));
        _ ->
            gen_tcp:send(Socket, "Unknown command\n"),
            loop(Nick, Socket)
    end.

say(Nick, Socket, Content) ->
    gen_server:cast(chat_handler, {say, Nick, Content}),
    loop(Nick, Socket).

clean(Data) ->
    re:replace(Data, "[\r\n]+$", "", [global, {return, list}]).
