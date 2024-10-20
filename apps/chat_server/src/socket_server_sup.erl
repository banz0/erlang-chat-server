%%%-------------------------------------------------------------------
%% @doc socket server supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(socket_server_sup).

-behaviour(supervisor).

-export([start_link/3, init/1]).

-define(SERVER, ?MODULE).

start_link(Name, Port, Loop) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Name, Port, Loop]).

init([Name, Port, Loop]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [{
        socket_server,
        {socket_server, start_link, [Name, Port, Loop]},
        permanent,
        5000,
        worker,
        [socket_server]
    }],
    {ok, {SupFlags, ChildSpecs}}.
