-module(splunkclient_http).
-include("splunkclient.hrl").
-export([init/1, terminate/2, get/3, get/4, get/5, post/3, post/4, post/5]).

%% ============================================================================
%% API functions
%% ============================================================================

init(C) ->
    Backend = C#splunkclient_conn.http_backend,
    {ok, _State} = Backend:init(C).

get(C, State, Path) ->
    get(C, State, Path, [], []).

get(C, State, Path, Params) ->
    get(C, State, Path, Params, []).

get(C, State, Path, Params, Headers) ->
    BaseUri = get_base_uri(C) ++ Path,
    Uri = case Params of
        [] ->
            BaseUri;
        _Else ->
            build_query_string(Params, BaseUri ++ "?")
    end,
    Backend = C#splunkclient_conn.http_backend,
    send_request(Backend, State, get, Uri, "", Headers, "").

post(C, State, Path) ->
    post(C, State, Path, [], []).

post(C, State, Path, Params) ->
    post(C, State, Path, Params, []).

post(C, State, Path, Params, Headers) ->
    Uri = get_base_uri(C) ++ Path,
    Type = "application/x-www-form-urlencoded",
    Body = build_query_string(Params),
    Backend = C#splunkclient_conn.http_backend,
    send_request(Backend, State, post, Uri, Body, Headers, Type).

terminate(C, State) ->
    Backend = C#splunkclient_conn.http_backend,
    ok = Backend:terminate(State).

%% ============================================================================
%% Internal functions
%% ============================================================================

get_base_uri(C) ->
    Protocol = C#splunkclient_conn.protocol,
    Host = C#splunkclient_conn.host,
    Port = C#splunkclient_conn.port,
    Uri = Protocol ++ "://" ++ Host ++ ":" ++ Port,
    Uri.

build_query_string(Params) ->
    build_query_string(Params, "").

build_query_string([], Acc) ->
    Acc;
build_query_string([{Name, Value}|Rest], Acc) ->
    build_query_string(
        Rest,
        Acc ++ "&" ++ http_uri:encode(Name) ++ "=" ++ http_uri:encode(Value)
    ).

send_request(Backend, State, Method, Uri, Body, Headers, Type) ->
    Backend:send_request(State, Method, Uri, Body, Headers, Type).
