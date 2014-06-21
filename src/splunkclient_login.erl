-module(splunkclient_login).

-include_lib("xmerl/include/xmerl.hrl").

-behaviour(gen_server).

%% Behavior callbacks
-export([start_link/2]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% User functions
-export([login/1]).
-export([get_token/1]).
-export([get_connection/1]).
-export([get_pool/1]).

%% State record
-record (state, {connection, http_state}).

%% ============================================================================
%% Supervision functions
%% ============================================================================

%% start with locally registered name, supplied from supervisor
start_link(Name, Conn) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Conn], []).

%% init, set state
init([Conn]) ->
    {ok, HttpState} = splunkclient_http:init(Conn),
    S = #state{connection = Conn, http_state = HttpState},
    {ok, S}.

%% ============================================================================
%% User functions
%% ============================================================================

login(Name) ->
    gen_server:call(Name, {login}).

get_token(Name) ->
    gen_server:call(Name, {get_token}).

get_connection(Name) ->
    gen_server:call(Name, {get_connection}).

% get worker pool for this connection (without sending a message to server)
get_pool(ConnectionName) ->
    {ok, Connections} = application:get_env(splunkclient, connections),
    Connection = proplists:get_value(ConnectionName, Connections),
    proplists:get_value(pool, Connection).

%% ============================================================================
%% Server functions
%% ============================================================================

handle_call({login}, _From, S) ->
    case liblogin(S#state.connection, S#state.http_state) of
        {ok, Token} ->
            gen_event:notify(splunkclient_service_eventman, {token, Token}),
            NewState = S#state{connection = splunkclient_conn:set_token(S#state.connection, Token)},
            {reply, ok, NewState};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({get_token}, _From, S) ->
    {reply, splunkclient_conn:get_token(S#state.connection), S};
handle_call({get_connection}, _From, S) ->
    {reply, S#state.connection, S}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, S) ->
    ok = splunkclient_http:terminate(S#state.connection, S#state.http_state).

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ============================================================================
%% Internal functions
%% ============================================================================

liblogin(C, HttpState) ->
    Body = [{"username", splunkclient_conn:get_user(C)},
            {"password", splunkclient_conn:get_pass(C)}],
    {ok, 200, _ResponseHeaders, ResponseBody} = splunkclient_http:post(C, HttpState, "/services/auth/login", [], [], Body),
    {XML, _} = xmerl_scan:string(binary_to_list(ResponseBody)),
    [#xmlText{value = SessionKey}] = xmerl_xpath:string("/response/sessionKey/text()", XML),
    {ok, "Splunk " ++ SessionKey}.

