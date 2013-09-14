-module(splunkclient_service).
-include("splunkclient.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-behaviour(gen_server).

%% Boilerplate
-export([start_link/0, start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% API
-export([oneshot_search/3]).

%% ============================================================================
%% Supervision functions
%% ============================================================================
%% start without name
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% start with module name
%% not used yet
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% init, set state
init([]) ->
    S = 0,
    {ok, S}.

%% ============================================================================
%% API functions
%% ============================================================================

oneshot_search(Name, Connection, SearchTerm) ->
    gen_server:call(Name, {oneshot_search, Connection, SearchTerm}).

%% ============================================================================
%% Server functions
%% ============================================================================

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

liboneshot_search(Connection, SearchTerm) ->
    Uri = splunkclient_http:get_base_uri(Connection) ++ "/services/search/jobs",
    Params = [{"exec_mode", "oneshot"}, {"search", "search " ++ SearchTerm}],
    Headers = [{"Authorization", Connection#splunkclient_conn.token}],
    {ok, ResponseBody} = splunkclient_http:post(Uri, Params, Headers),
    %{XML, _} = xmerl_scan:string(ResponseBody),
    %io:fwrite("got xml result: ~s~n", [XML]),
    {ok, ResponseBody}.

