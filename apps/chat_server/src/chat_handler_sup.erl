%%%-------------------------------------------------------------------
%% @doc chat handler supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_handler_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [{
        chat_handler,
        {chat_handler, start_link, []},
        permanent,
        5000,
        worker,
        [chat_handler]
    }],
    {ok, {SupFlags, ChildSpecs}}.
