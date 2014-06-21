-module(splunkclient_login_sup).

-behaviour(supervisor).

%% User functions
-export([start_link/1]).

%% Behavior callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% ============================================================================
%% User functions
%% ============================================================================

start_link([Connections]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Connections]).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

%% Spawn login gen_servers, one per configured connection.
%% Connection name will be the registered name of the gen_server.
init([Connections]) ->
    F = fun({Name, Connection}) ->
        ?CHILD(Name, splunkclient_login, [Name, Connection], worker)
        end,
    LoginWorkers = lists:map(F, Connections),
    {ok, {{one_for_one, 5, 10}, LoginWorkers}}.
