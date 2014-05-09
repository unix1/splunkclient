-module(splunkclient_service_sup).
-include("splunkclient.hrl").
-behaviour(supervisor).

%% Boilerplate
-export([start_link/0, init/1]).

%% ============================================================================
%% API functions
%% ============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ============================================================================
%% Supervisor callbacks
%% ============================================================================

init([]) ->
    % supervisor auto-starts configured poolboy pools
    {ok, Pools} = application:get_env(splunkclient, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, splunkclient_service}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, PoolSpecs}}.
