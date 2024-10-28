%%%-------------------------------------------------------------------
%% @doc server that handles the chat business logic
%% @end
%%%-------------------------------------------------------------------

-module(chat_handler).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {users, rooms}).


start_link() ->
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
            {ok, room_list(NewRooms)}
    end,
    {reply, Response, State#state{rooms=NewRooms}};

handle_call({list_rooms, _Nick}, _From, State = #state{rooms=Rooms}) ->
    Response = case dict:is_empty(Rooms) of
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
            IsMember = lists:member([Nick], Members),
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
    end;

handle_call({leave_room, Nick, RoomName}, _From, State = #state{rooms=Rooms}) ->
    case dict:is_key(RoomName, Rooms) of
        false ->
            {reply, {error, room_doesnt_exist}, State};
        true ->
            % check if user is in room and if so remove him from the room members
            % if the user is the room creator
            Members = dict:fetch(RoomName, Rooms),
            IsMember = lists:member([Nick], Members),
            [Creator | _] = Members,
            IsCreator = Creator == [Nick],
            Response = if IsCreator ->
                            NewRooms = Rooms,
                            {error, is_creator};
                        IsMember ->
                            UpdatedMembers = lists:filter(fun(Member) -> Member =/= [Nick] end, Members),
                            NewRooms = dict:store(RoomName, UpdatedMembers, Rooms),
                            {ok, members_list(UpdatedMembers)};
                        true ->
                            NewRooms = Rooms,
                            {error, not_member}
            end,
            {reply, Response, State#state{rooms=NewRooms}}
    end;

handle_call({destroy_room, Nick, RoomName}, _From, State = #state{rooms=Rooms}) ->
    case dict:is_key(RoomName, Rooms) of
        false ->
            {reply, no_room, State};
        true ->
            % check if user is room owner, and if so delete the room
            Members = dict:fetch(RoomName, Rooms),
            [Creator | _] = Members,
            IsCreator = Creator == [Nick],
            Response = if IsCreator ->
                            UpdatedRooms = dict:erase(RoomName, Rooms),
                            {ok, room_list(UpdatedRooms)};
                        true ->
                            UpdatedRooms = Rooms,
                            not_creator
            end,
            {reply, Response, State#state{rooms=UpdatedRooms}}
    end.


handle_cast({say, Nick, RoomName, Msg}, State = #state{rooms=Rooms, users=Users}) ->
    case is_member(Nick, RoomName, Rooms) of
        {ok, Members} ->
            Message = "ROOM:" ++ RoomName ++ ":USER:" ++ Nick ++ ":SAID:" ++ Msg ++ "\n",
            Members = dict:fetch(RoomName, Rooms),
            broadcast_to_room(Nick, Message, Users, Members),
            {noreply, State};
        {error, _} ->
            {noreply, State}
    end;

handle_cast({create, Nick, RoomName}, State = #state{users=Users}) ->
    Message = "ROOM:" ++ RoomName ++ ":CREATED" ++ "\n",
    broadcast(Nick, Message, Users),
    {noreply, State};

handle_cast({destroy, Nick, RoomName}, State = #state{users=Users}) ->
    Message = "ROOM:" ++ RoomName ++ ":DESTROYED" ++ "\n",
    broadcast(Nick, Message, Users),
    {noreply, State};

handle_cast({join, Nick, RoomName}, State = #state{rooms=Rooms, users=Users}) ->
    Message = "ROOM:" ++ RoomName ++ ":USER:" ++ Nick ++ ":JOINED" ++ "\n",
    Members = dict:fetch(RoomName, Rooms),
    broadcast_to_room(Nick, Message, Users, Members),
    {noreply, State};

handle_cast({leave, Nick, RoomName}, State = #state{rooms=Rooms, users=Users}) ->
    Message = "ROOM:" ++ RoomName ++ ":USER:" ++ Nick ++ ":LEFT" ++ "\n",
    Members = dict:fetch(RoomName, Rooms),
    broadcast_to_room(Nick, Message, Users, Members),
    {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.


% aux functions

broadcast(Nick, Msg, Users) ->
    Sockets = lists:map(fun({_, [Value|_]}) -> Value end, dict:to_list(dict:erase(Nick, Users))),
    lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Msg) end, Sockets).

broadcast_to_room(Nick, Msg, Users, RoomMembers) ->
    % the flattening is really ugly, but I haven't found another solution
    FlatMembers = lists:flatmap(fun(Member) -> Member end, RoomMembers),
    MemberSockets = [Socket || Member <- FlatMembers, {ok, Socket} <- [dict:find(Member, dict:erase(Nick, Users))]],
    FlatSockets = lists:flatmap(fun(Socket) -> Socket end, MemberSockets),
    lists:foreach(fun(Socket) -> gen_tcp:send(Socket, Msg) end, FlatSockets).

user_list(Users) ->
    UserList = dict:fetch_keys(Users),
    string:join(UserList, ":").

room_list(Rooms) ->
    RoomList = dict:fetch_keys(Rooms),
    string:join(RoomList, ":").

members_list(Members) -> string:join(Members, ":").

is_member(Nick, RoomName, Rooms) ->
    case dict:find(RoomName, Rooms) of
        {ok, Members} ->
            IsMember = lists:member([Nick], Members),
            if IsMember ->
                    {ok, Members};
                true ->
                    {error, not_member}
            end;
        error ->
            {error, no_room}
    end.


%% dummy implementations to suppress warnings

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
