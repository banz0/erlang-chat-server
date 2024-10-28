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
    chat_handler:start(), % TODO use supervisor to start link
    socket_server_sup:start_link(?MODULE, Port,  {?MODULE, pre_loop}).

stop(_State) ->
    ok.

pre_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("[pre_loop] Received data: ~p~n", [binary_to_list(Data)]),
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
            io:format("[loop] Received data: ~p~n", [Data]),
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
        "CREATE_ROOM" ->
            create_room(Nick, Socket, clean(Content));
        "LIST_ROOMS" ->
            list_rooms(Nick, Socket);
        "JOIN_ROOM" ->
            join_room(Nick, Socket, clean(Content));
        _ ->
            gen_tcp:send(Socket, "Unknown command\n"),
            loop(Nick, Socket)
    end.


% commands

say(Nick, Socket, Content) ->
    gen_server:cast(chat_handler, {say, Nick, Content}),
    loop(Nick, Socket).

create_room(Nick, Socket, Content) ->
    Response = gen_server:call(chat_handler, {create_room, Nick, clean(Content)}),
    case Response of
        {ok, List} ->
            gen_tcp:send(Socket, "CREATE_ROOM:OK:" ++ List ++ "\n");
        room_already_exists ->
            gen_tcp:send(Socket, "CREATE_ROOM:ERROR:Room already exists.\n"),
            ok
    end,
    loop(Nick, Socket).

list_rooms(Nick, Socket) ->
    Response = gen_server:call(chat_handler, {list_rooms, Nick}),
    case Response of
        {ok, List} ->
            gen_tcp:send(Socket, "LIST_ROOMS:OK:" ++ List ++ "\n");
        no_rooms ->
            gen_tcp:send(Socket, "LIST_ROOMS:ERROR:No rooms available.\n"),
            ok
    end,
    loop(Nick, Socket).

join_room(Nick, Socket, RoomName) ->
    Response = gen_server:call(chat_handler, {join_room, Nick, RoomName}),
    case Response of
        {ok, List} ->
            gen_tcp:send(Socket, "JOIN_ROOM:OK:" ++ List ++ "\n");
            % gen_server:cast(chat_handler, {join, RoomName, Nick}),
        already_joined ->
            gen_tcp:send(Socket, "JOIN_ROOM:ERROR:You're already in the room.\n"),
            ok;
        room_doesnt_exist ->
            gen_tcp:send(Socket, "JOIN_ROOM:ERROR:The room you're trying to join doesn't exist.\n"),
            ok
    end,
    loop(Nick, Socket).

% aux functions

clean(Data) ->
    re:replace(Data, "[\r\n]+$", "", [global, {return, list}]).
