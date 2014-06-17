-module(splunkclient).

-include("splunkclient.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

%% User functions
-export([start/0]).
-export([stop/0]).
-export([login/0, login/1]).
-export([get_indexes/0, get_indexes/1]).
-export([get_jobs/0, get_jobs/1]).
-export([get_saved_searches/0, get_saved_searches/1]).
-export([oneshot_search/1, oneshot_search/2]).

%% ============================================================================
%% Application callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    {ok, ConnectionsConfig} = application:get_env(connections),
    F = fun({Name, [{protocol, Pr}, {host, H}, {port, Po}, {user, U}, {pass, Pa}, {pool, Pl}, {http_backend, Hb}]}) ->
        {Name, #splunkclient_conn{protocol = Pr, host = H, port = Po, user = U, pass = Pa, pool = Pl, http_backend = Hb}}
        end,
    Connections = lists:map(F, ConnectionsConfig),
    splunkclient_sup:start_link(Connections).

stop(_State) ->
    ok.

%% ============================================================================
%% User functions
%% ============================================================================

start() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(splunkclient).

stop() ->
    application:stop(splunkclient),
    application:stop(inets),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(asn1),
    application:stop(crypto).

get_indexes() ->
    get_indexes(splunkclient_conn_default).

get_indexes(Connection) ->
    splunkclient_service:get_indexes(Connection).

get_jobs() ->
    get_jobs(splunkclient_conn_default).

get_jobs(Connection) ->
    splunkclient_service:get_jobs(Connection).

get_saved_searches() ->
    get_saved_searches(splunkclient_conn_default).

get_saved_searches(Connection) ->
    splunkclient_service:get_saved_searches(Connection).

login() ->
    login(splunkclient_conn_default).

login(Name) ->
    splunkclient_login:login(Name).

oneshot_search(Term) ->
    oneshot_search(splunkclient_conn_default, Term).

oneshot_search(Connection, Term) ->
    splunkclient_service:oneshot_search(Connection, Term).
