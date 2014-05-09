-module(splunkclient_service).
-include("splunkclient.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-behaviour(gen_server).

%% Boilerplate
-export([%start_link/0,
         start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% API
-export([get_indexes/1, get_jobs/1, get_saved_searches/1, oneshot_search/2]).

%% ============================================================================
%% Supervision functions
%% ============================================================================
%% start without name
start_link([]) ->
    gen_server:start_link(?MODULE, [], []).

%% init, set state
init([]) ->
    S = 0,
    {ok, S}.

%% ============================================================================
%% API functions
%% ============================================================================

get_indexes(Connection) ->
    Worker = poolboy:checkout(splunkclient_pool_service),
    Result = gen_server:call(Worker, {get_indexes, Connection}),
    poolboy:checkin(splunkclient_pool_service, Worker),
    Result.

get_jobs(Connection) ->
    Worker = poolboy:checkout(splunkclient_pool_service),
    Result = gen_server:call(Worker, {get_jobs, Connection}),
    poolboy:checkin(splunkclient_pool_service, Worker),
    Result.

get_saved_searches(Connection) ->
    Worker = poolboy:checkout(splunkclient_pool_service),
    Result = gen_server:call(Worker, {get_saved_searches, Connection}),
    poolboy:checkin(splunkclient_pool_service, Worker),
    Result.

oneshot_search(Connection, SearchTerm) ->
    Worker = poolboy:checkout(splunkclient_pool_service),
    Result = gen_server:call(Worker, {oneshot_search, Connection, SearchTerm}),
    poolboy:checkin(splunkclient_pool_service, Worker),
    Result.

%% ============================================================================
%% Server functions
%% ============================================================================

handle_call({get_indexes, ConnectionName}, _From, S) ->
    C = splunkclient_login:get_connection(ConnectionName),
    case libget_indexes(C) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({get_jobs, ConnectionName}, _From, S) ->
    C = splunkclient_login:get_connection(ConnectionName),
    case libget_jobs(C) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({get_saved_searches, ConnectionName}, _From, S) ->
    C = splunkclient_login:get_connection(ConnectionName),
    case libget_saved_searches(C) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknwon error", S}
    end;
handle_call({oneshot_search, ConnectionName, SearchTerm}, _From, S) ->
    C = splunkclient_login:get_connection(ConnectionName),
    case liboneshot_search(C, SearchTerm) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ============================================================================
%% Internal functions
%% ============================================================================

libget_indexes(Connection) ->
    Uri = splunkclient_http:get_base_uri(Connection) ++ "/services/data/indexes/",
    Params = [],
    Headers = [{"Authorization", Connection#splunkclient_conn.token}],
    {ok, ResponseBody} = splunkclient_http:get(Uri, Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

libget_jobs(Connection) ->
    Uri = splunkclient_http:get_base_uri(Connection) ++ "/services/search/jobs/",
    Params = [],
    Headers = [{"Authorization", Connection#splunkclient_conn.token}],
    {ok, ResponseBody} = splunkclient_http:get(Uri, Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

libget_saved_searches(Connection) ->
    Uri = splunkclient_http:get_base_uri(Connection) ++ "/services/saved/searches/",
    Params = [],
    Headers = [{"Authorization", Connection#splunkclient_conn.token}],
    {ok, ResponseBody} = splunkclient_http:get(Uri, Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

liboneshot_search(Connection, SearchTerm) ->
    Uri = splunkclient_http:get_base_uri(Connection) ++ "/services/search/jobs/",
    Params = [{"exec_mode", "oneshot"}, {"search", "search " ++ SearchTerm}],
    Headers = [{"Authorization", Connection#splunkclient_conn.token}],
    {ok, ResponseBody} = splunkclient_http:post(Uri, Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

