%%%-------------------------------------------------------------------
%% @doc server that handles the chat business logic
%% @end
%%%-------------------------------------------------------------------

-module(chat_handler).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users, rooms}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Users = dict:new(), % maps users nickames to sockets
    Rooms = dict:new(), % maps rooms to their members nicknames
    {ok, #state{users=Users, rooms=Rooms}}.

handle_call({connect, Nick, Socket}, _From, State = #state{users=Users}) ->
    Response = case dict:is_key(Nick, Users) of
        true ->
            NewUsers = Users,
            nick_in_use;
        false ->
            NewUsers = dict:append(Nick, Socket, Users),
            {ok, user_list(NewUsers)}
    end,
    {reply, Response, State#state{users=NewUsers}};

handle_call({create_room, Nick, RoomName}, _From, State = #state{rooms=Rooms}) ->
    Response = case dict:is_key(RoomName, Rooms) of
        true ->
            NewRooms = Rooms,
            room_already_exists;  % room already exists, ignore request
        false ->
            NewRooms = dict:append(RoomName, [Nick], Rooms), % TODO figure out a way too track room ownership
            io:format("Room members: ~p~n", [members_list([Nick])]),
            {ok, room_list(NewRooms)}
    end,
    {reply, Response, State#state{rooms=NewRooms}};

handle_call({list_rooms, _Nick}, _From, State = #state{rooms=Rooms}) ->
    Response = case is_empty(Rooms) of
        true ->
            no_rooms;
        false ->
            {ok, room_list(Rooms)}
    end,
    {reply, Response, State#state{rooms=Rooms}};

handle_call({join_room, Nick, RoomName}, _From, State = #state{rooms=Rooms}) ->
    case dict:is_key(RoomName, Rooms) of
        false ->
            {reply, room_doesnt_exist, State};
        true ->
            % check if user is in list
            Members = dict:fetch(RoomName, Rooms),
            io:format("Room members: ~p~n", [Members]),
            io:format("Nick: ~p~n", [Nick]),
            IsMember = lists:member([Nick], Members),
            io:format("Is member: ~p~n", [IsMember]),
            Response = if IsMember ->
                            NewRooms = Rooms,
                            already_joined;
                        true ->
                            % add user to room members
                            UpdatedMembers = Members ++ [[Nick]],
                            NewRooms = dict:store(RoomName, UpdatedMembers, Rooms),
                            {ok, members_list(UpdatedMembers)}
            end,
            {reply, Response, State#state{rooms=NewRooms}}
    end.

handle_cast({say, Nick, Msg}, State = #state{users=Users}) ->
    broadcast(Nick, "SAID:" ++ Nick ++ ":" ++ Msg ++ "\n", Users),
    {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.


% aux functions

broadcast(Nick, Msg, Users) ->
    Sockets = lists:map(fun({_, [Value|_]}) -> Value end, dict:to_list(dict:erase(Nick, Users))),
    lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Msg) end, Sockets).

user_list(Users) ->
    UserList = dict:fetch_keys(Users),
    string:join(UserList, ":").

room_list(Rooms) ->
    RoomList = dict:fetch_keys(Rooms),
    io:format("RoomList: ~p~n", [RoomList]),
    string:join(RoomList, ":").

members_list(Members) -> string:join(Members, ":").

is_empty([]) -> true;

is_empty(_) -> false.


%% dummy implementations to suppress warnings

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
