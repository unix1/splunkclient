-module(splunkclient_login).
-include("splunkclient.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-behaviour(gen_server).

%% Boilerplate
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([login/1, get_token/1, get_connection/1, get_pool/1]).

%% ============================================================================
%% Supervision functions
%% ============================================================================

%% start with locally registered name, supplied from supervisor
start_link(Name, Connection) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Connection], []).

%% init, set state
init([Connection]) ->
    S = Connection,
    {ok, S}.

%% ============================================================================
%% API functions
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
    case liblogin(S) of
        {ok, Token} ->
            gen_event:notify(splunkclient_service_eventman, {token, Token}),
            NewState = S#splunkclient_conn{token = Token},
            {reply, ok, NewState};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({get_token}, _From, S) ->
    {reply, S#splunkclient_conn.token, S};
handle_call({get_connection}, _From, S) ->
    {reply, S, S}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ============================================================================
%% Internal functions
%% ============================================================================

liblogin(C) ->
    Params = [{"username", C#splunkclient_conn.user},
              {"password", C#splunkclient_conn.pass}],
    {ok, ResponseBody} = splunkclient_http:post(C, "/services/auth/login", Params),
    {XML, _} = xmerl_scan:string(ResponseBody),
    [#xmlText{value = SessionKey}] = xmerl_xpath:string("/response/sessionKey/text()", XML),
    {ok, "Splunk " ++ SessionKey}.

