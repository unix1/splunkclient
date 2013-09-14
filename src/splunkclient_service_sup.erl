-module(splunkclient_service_sup).
-include("splunkclient.hrl").
-behaviour(supervisor).

%% Boilerplate
-export([start_link/0, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% ============================================================================
%% API functions
%% ============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ============================================================================
%% Supervisor callbacks
%% ============================================================================

init([]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    ChildSpec = ?CHILD(splunkclient_service, splunkclient_service, [], worker),
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.

