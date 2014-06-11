-module(splunkclient_http).
-include("splunkclient.hrl").
-export([init/1, terminate/2, get/3, get/4, get/5, post/3, post/4, post/5]).

%% ============================================================================
%% API functions
%% ============================================================================

init(C) ->
    Backend = C#splunkclient_conn.http_backend,
    Protocol = C#splunkclient_conn.protocol,
    Host = C#splunkclient_conn.host,
    Port = C#splunkclient_conn.port,
    {ok, _State} = Backend:init(Protocol, Host, Port).

get(C, State, Path) ->
    get(C, State, Path, [], []).

get(C, State, Path, Params) ->
    get(C, State, Path, Params, []).

get(C, State, BasePath, Params, Headers) ->
    Path = case Params of
        [] ->
            BasePath;
        _Else ->
            build_query_string(Params, BasePath ++ "?")
    end,
    Backend = C#splunkclient_conn.http_backend,
    Request = get_request(get, C, Path, "", Headers, ""),
    send_request(Backend, State, Request).

post(C, State, Path) ->
    post(C, State, Path, [], []).

post(C, State, Path, Params) ->
    post(C, State, Path, Params, []).

post(C, State, Path, Params, Headers) ->
    Type = "application/x-www-form-urlencoded",
    Body = build_query_string(Params),
    Backend = C#splunkclient_conn.http_backend,
    Request = get_request(post, C, Path, Body, Headers, Type),
    send_request(Backend, State, Request).

terminate(C, State) ->
    Backend = C#splunkclient_conn.http_backend,
    ok = Backend:terminate(State).

%% ============================================================================
%% Internal functions
%% ============================================================================

get_request(Method, C, Path, Body, Headers, Type) ->
    #splunkclient_http{method = Method,
                       protocol = C#splunkclient_conn.protocol,
                       host = C#splunkclient_conn.host,
                       port = C#splunkclient_conn.port,
                       path = Path,
                       body = Body,
                       headers = Headers,
                       type = Type}.

build_query_string(Params) ->
    build_query_string(Params, "").

build_query_string([], Acc) ->
    Acc;
build_query_string([{Name, Value}|Rest], "") ->
    build_query_string(
        Rest,
        http_uri:encode(Name) ++ "=" ++ http_uri:encode(Value));
build_query_string([{Name, Value}|Rest], Acc) ->
    build_query_string(
        Rest,
        Acc ++ "&" ++ http_uri:encode(Name) ++ "=" ++ http_uri:encode(Value)
    ).

send_request(Backend, State, Request) ->
    Backend:send_request(State, Request).
