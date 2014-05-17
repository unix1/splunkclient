-module(splunkclient_service).
-include("splunkclient.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-behaviour(gen_server).

%% Boilerplate
-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% API
-export([get_indexes/1, get_jobs/1, get_saved_searches/1, oneshot_search/2]).
-export([update_connection_token/2]).

%% state record
-record (state, {connection = #splunkclient_conn{}, event_handler_id}).

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
    Connection1 = splunkclient_login:get_connection(ConnectionName),
    % override default connection http backend from service pool configuration
    Connection2 = Connection1#splunkclient_conn{http_backend = HttpBackend},
    splunkclient_http:init(Connection2),
    S = #state{connection = Connection2, event_handler_id = HandlerId},
    {ok, S}.

%% ============================================================================
%% User API functions
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

update_connection_token(Pid, Token) ->
    gen_server:call(Pid, {update_token, Token}).

%% ============================================================================
%% Server functions
%% ============================================================================

handle_call({get_indexes}, _From, S) ->
    Connection = S#state.connection,
    case libget_indexes(Connection) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({get_jobs}, _From, S) ->
    Connection = S#state.connection,
    case libget_jobs(Connection) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({get_saved_searches}, _From, S) ->
    Connection = S#state.connection,
    case libget_saved_searches(Connection) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknwon error", S}
    end;
handle_call({oneshot_search, SearchTerm}, _From, S) ->
    Connection = S#state.connection,
    case liboneshot_search(Connection, SearchTerm) of
        {ok, Results} ->
            {reply, {ok, search_results, Results}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({update_token, Token}, _From, S) ->
    {reply, ok, S#state{connection = S#state.connection#splunkclient_conn{token = Token}}}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, S) ->
    gen_event:delete_handler(splunkclient_service_eventman, S#state.event_handler_id, []),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ============================================================================
%% Internal functions
%% ============================================================================

libget_indexes(C) ->
    Uri = "/services/data/indexes/",
    Params = [],
    Headers = [{"Authorization", C#splunkclient_conn.token}],
    {ok, ResponseBody} = splunkclient_http:get(C, Uri, Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

libget_jobs(C) ->
    Uri = "/services/search/jobs/",
    Params = [],
    Headers = [{"Authorization", C#splunkclient_conn.token}],
    {ok, ResponseBody} = splunkclient_http:get(C, Uri, Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

libget_saved_searches(C) ->
    Uri = "/services/saved/searches/",
    Params = [],
    Headers = [{"Authorization", C#splunkclient_conn.token}],
    {ok, ResponseBody} = splunkclient_http:get(C, Uri, Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

liboneshot_search(C, SearchTerm) ->
    Uri = "/services/search/jobs/",
    Params = [{"exec_mode", "oneshot"}, {"search", "search " ++ SearchTerm}],
    Headers = [{"Authorization", C#splunkclient_conn.token}],
    {ok, ResponseBody} = splunkclient_http:post(C, Uri, Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

