-module(splunkclient_sup).

-behaviour(supervisor).

%% User functions
-export([start_link/1]).

%% Behavior callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% ===================================================================
%% User functions
%% ===================================================================

start_link(Connections) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Connections]).

%% ===================================================================
%% Behavior callbacks
%% ===================================================================

init(Connections) ->
    LoginSup = ?CHILD(splunkclient_login_sup, splunkclient_login_sup, [Connections], supervisor),
    ServiceSup = ?CHILD(splunkclient_service_sup, splunkclient_service_sup, [], supervisor),
    {ok, {{one_for_one, 5, 10}, [LoginSup, ServiceSup]}}.
