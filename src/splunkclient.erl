-module(splunkclient).

-behaviour(application).

%% Behavior callbacks
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
-export([send_simple/2, send_simple/3]).

-define(DEFAULT_CONN, default).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    {ok, ConnectionsConfig} = application:get_env(connections),
    {ok, PoolConfig} = application:get_env(service_pools),
    httpclient:start_pool(ConnectionsConfig, PoolConfig),
    splunkclient_sup:start_link().

stop(_State) ->
    ok.

%% ============================================================================
%% User functions
%% ============================================================================

start() ->
    ok = application:ensure_started(ranch),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(inets),
    ok = application:ensure_started(poolboy),
    ok = application:ensure_started(gun),
    ok = application:ensure_started(httpclient),
    ok = application:ensure_started(splunkclient),
    ok.

stop() ->
    ok = application:stop(splunkclient),
    ok = application:stop(httpclient),
    ok = application:stop(gun),
    ok = application:stop(poolboy),
    ok = application:stop(inets),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(asn1),
    ok = application:stop(cowlib),
    ok = application:stop(crypto),
    ok = application:stop(ranch),
    ok.

get_indexes() ->
    get_indexes(?DEFAULT_CONN).

get_indexes(Connection) ->
    splunkclient_service:get_indexes(Connection).

get_jobs() ->
    get_jobs(?DEFAULT_CONN).

get_jobs(Connection) ->
    splunkclient_service:get_jobs(Connection).

get_saved_searches() ->
    get_saved_searches(?DEFAULT_CONN).

get_saved_searches(Connection) ->
    splunkclient_service:get_saved_searches(Connection).

login() ->
    login(?DEFAULT_CONN).

login(Name) ->
    httpclient_login:login(Name).

oneshot_search(Term) ->
    oneshot_search(?DEFAULT_CONN, Term).

oneshot_search(Connection, Term) ->
    splunkclient_service:oneshot_search(Connection, Term).

send_simple(Event, Params) ->
    send_simple(?DEFAULT_CONN, Event, Params).

send_simple(Connection, Event, Params) ->
    splunkclient_service:send_simple(Connection, Event, Params).
