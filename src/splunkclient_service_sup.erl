-module(splunkclient_service_sup).

-behaviour(supervisor).

%% User functions
-export([start_link/0]).

%% Behavior callbacks
-export([init/1]).

%% ============================================================================
%% User functions
%% ============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

init([]) ->
    % start event manager
    {ok, _Pid} = gen_event:start_link({local, splunkclient_service_eventman}),
    % supervisor auto-starts configured poolboy pools
    {ok, Pools} = application:get_env(splunkclient, service_pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, splunkclient_service}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, PoolSpecs}}.
