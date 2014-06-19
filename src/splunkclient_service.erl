-module(splunkclient_service).

-include("splunkclient.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-behaviour(gen_server).

%% Behavior callbacks
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% User functions
-export([get_indexes/1]).
-export([get_jobs/1]).
-export([get_saved_searches/1]).
-export([oneshot_search/2]).
-export([send_simple/3]).
-export([update_connection_token/2]).

%% State record
-record (state, {connection, event_handler_id, http_state}).

%% ============================================================================
%% Supervision functions
%% ============================================================================
%% start without name
start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

%% gen_server init, set state
init([Config]) ->
    % add event handler
    HandlerId = {splunkclient_service_event, make_ref()},
    gen_event:add_handler(splunkclient_service_eventman, HandlerId, [self()]),
    ConnectionName = proplists:get_value(connection, Config),
    HttpBackend = proplists:get_value(http_backend, Config),
    Conn1 = splunkclient_login:get_connection(ConnectionName),
    % override default connection http backend from service pool configuration
    Conn2 = splunkclient_conn:set_backend(Conn1, HttpBackend),
    {ok, HttpState} = splunkclient_http:init(Conn2),
    S = #state{connection = Conn2, event_handler_id = HandlerId,
        http_state = HttpState},
    {ok, S}.

%% ============================================================================
%% User functions
%% ============================================================================

get_indexes(ConnectionName) ->
    Pool = splunkclient_login:get_pool(ConnectionName),
    Worker = poolboy:checkout(Pool),
    Result = gen_server:call(Worker, {get_indexes}),
    poolboy:checkin(Pool, Worker),
    Result.

get_jobs(ConnectionName) ->
    Pool = splunkclient_login:get_pool(ConnectionName),
    Worker = poolboy:checkout(Pool),
    Result = gen_server:call(Worker, {get_jobs}),
    poolboy:checkin(Pool, Worker),
    Result.

get_saved_searches(ConnectionName) ->
    Pool = splunkclient_login:get_pool(ConnectionName),
    Worker = poolboy:checkout(Pool),
    Result = gen_server:call(Worker, {get_saved_searches}),
    poolboy:checkin(Pool, Worker),
    Result.

oneshot_search(ConnectionName, SearchTerm) ->
    Pool = splunkclient_login:get_pool(ConnectionName),
    Worker = poolboy:checkout(Pool),
    Result = gen_server:call(Worker, {oneshot_search, SearchTerm}),
    poolboy:checkin(Pool, Worker),
    Result.

send_simple(ConnectionName, Event, Params) ->
    Pool = splunkclient_login:get_pool(ConnectionName),
    Worker = poolboy:checkout(Pool),
    Result = gen_server:call(Worker, {send_simple, Event, Params}),
    poolboy:checkin(Pool, Worker),
    Result.

update_connection_token(Pid, Token) ->
    gen_server:call(Pid, {update_token, Token}).

%% ============================================================================
%% Server functions
%% ============================================================================

handle_call({get_indexes}, _From, S) ->
    Conn = S#state.connection,
    HttpState = S#state.http_state,
    case libget_indexes(Conn, HttpState) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({get_jobs}, _From, S) ->
    Conn = S#state.connection,
    HttpState = S#state.http_state,
    case libget_jobs(Conn, HttpState) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({get_saved_searches}, _From, S) ->
    Conn = S#state.connection,
    HttpState = S#state.http_state,
    case libget_saved_searches(Conn, HttpState) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknwon error", S}
    end;
handle_call({oneshot_search, SearchTerm}, _From, S) ->
    Conn = S#state.connection,
    HttpState = S#state.http_state,
    case liboneshot_search(Conn, HttpState, SearchTerm) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({send_simple, Event, Params}, _From, S) ->
    Conn = S#state.connection,
    HttpState = S#state.http_state,
    case libsend_simple(Conn, HttpState, Event, Params) of
        ok ->
            {reply, ok, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({update_token, Token}, _From, S) ->
    Conn = splunkclient_conn:set_token(S#state.connection, Token),
    {reply, ok, S#state{connection = Conn}}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, S) ->
    gen_event:delete_handler(splunkclient_service_eventman,
        S#state.event_handler_id, []),
    ok = splunkclient_http:terminate(S#state.connection, S#state.http_state).

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ============================================================================
%% Internal functions
%% ============================================================================

libget_indexes(C, HttpState) ->
    Path = "/services/data/indexes/",
    Params = [],
    Headers = [{"Authorization", splunkclient_conn:get_token(C)}],
    {ok, 200, _, ResponseBody} = splunkclient_http:get(C, HttpState, Path,
        Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

libget_jobs(C, HttpState) ->
    Path = "/services/search/jobs/",
    Params = [],
    Headers = [{"Authorization", splunkclient_conn:get_token(C)}],
    {ok, 200, _, ResponseBody} = splunkclient_http:get(C, HttpState, Path,
        Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

libget_saved_searches(C, HttpState) ->
    Path = "/services/saved/searches/",
    Params = [],
    Headers = [{"Authorization", splunkclient_conn:get_token(C)}],
    {ok, 200, _, ResponseBody} = splunkclient_http:get(C, HttpState, Path,
        Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

liboneshot_search(C, HttpState, SearchTerm) ->
    Path = "/services/search/jobs/",
    Body = [{"exec_mode", "oneshot"}, {"search", "search " ++ SearchTerm}],
    Headers = [{"Authorization", splunkclient_conn:get_token(C)}],
    {ok, 200, _, ResponseBody} = splunkclient_http:post(C, HttpState, Path,
        [], Headers, Body),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

libsend_simple(C, HttpState, Event, Params) ->
    Path = "/services/receivers/simple",
    Headers = [{"Authorization", splunkclient_conn:get_token(C)}],
    {ok, 200, _, _} = splunkclient_http:post(C, HttpState, Path, Params,
        Headers, Event),
    ok.
