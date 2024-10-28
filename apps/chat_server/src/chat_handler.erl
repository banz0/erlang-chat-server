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


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% This is called when a connection is made to the server
init([]) ->
    Users = dict:new(), % Maps users nickames to sockets
    {ok, Users}.

handle_call({connect, Nick, Socket}, _From, Users) ->
    Response = case dict:is_key(Nick, Users) of
        true ->
            NewUsers = Users,
            nick_in_use;
        false ->
            NewUsers = dict:append(Nick, Socket, Users),
            {ok, user_list(NewUsers)}
    end,
    {reply, Response, NewUsers}.

user_list(Users) ->
    UserList = dict:fetch_keys(Users),
    string:join(UserList, ":").

handle_cast({say, Nick, Msg}, Users) ->
    broadcast(Nick, "SAID:" ++ Nick ++ ":" ++ Msg ++ "\n", Users),
    {noreply, Users};
handle_cast(_Request, State) -> {noreply, State}.

% auxiliary functions
broadcast(Nick, Msg, Users) ->
    Sockets = lists:map(fun({_, [Value|_]}) -> Value end, dict:to_list(dict:erase(Nick, Users))),
    lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Msg) end, Sockets).

%% dummy implementations to suppress warnings

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
