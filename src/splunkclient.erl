-module(splunkclient).
-include("splunkclient.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% User functions
-export([start/0, login/0, login/1, oneshot_search/1, oneshot_search/2]).

%% ============================================================================
%% Application callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    {ok, ConnectionsConfig} = application:get_env(connections),
    F = fun({Name, {protocol, Pr}, {host, H}, {port, Po}, {user, U}, {pass, Pa}}) ->
        {Name, #splunkclient_conn{protocol = Pr, host = H, port = Po, user = U, pass = Pa}}
        end,
    Connections = lists:map(F, ConnectionsConfig),
    splunkclient_sup:start_link(Connections).

stop(_State) ->
    ok.

%% ============================================================================
%% API functions
%% ============================================================================

start() ->
    start_dependencies(),
    application:start(splunkclient).

login() ->
    login(splunkclient_conn_default).

login(Name) ->
    splunkclient_login:login(Name).

oneshot_search(Term) ->
    oneshot_search(splunkclient_conn_default, Term).

oneshot_search(Connection, Term) ->
    {ok, Pid} = supervisor:start_child(splunkclient_service_sup, []),
    Result = splunkclient_service:oneshot_search(Pid, Connection, Term),
    supervisor:terminate_child(splunkclient_service_sup, Pid),
    supervisor:delete_child(splunkclient_service_sup, Pid),
    Result.

%% ============================================================================
%% Internal functions
%% ============================================================================

start_dependencies() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets).

