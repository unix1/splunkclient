-module(splunkclient_login_sup).

-include("splunkclient.hrl").

-behaviour(supervisor).

%% Boilerplate
-export([start_link/1]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% ============================================================================
%% API functions
%% ============================================================================

start_link([Connections]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Connections]).

%% ============================================================================
%% Supervisor callbacks
%% ============================================================================

%% Spawn login gen_servers, one per configured connection.
%% Connection name will be the registered name of the gen_server.
init([Connections]) ->
    F = fun({Name, Connection}) ->
        ?CHILD(Name, splunkclient_login, [Name, Connection], worker)
        end,
    LoginWorkers = lists:map(F, Connections),
    {ok,
        {
            {one_for_one, 5, 10},
            LoginWorkers
        }
    }.

