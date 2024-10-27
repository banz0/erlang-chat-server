%%%-------------------------------------------------------------------
%% @doc server that handles the chat business logic
%% @end
%%%-------------------------------------------------------------------

-module(chat_handler).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start() ->
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

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
