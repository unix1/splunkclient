-module(splunkclient_http).

-include("splunkclient.hrl").

%% User functions
-export([init/1]).
-export([terminate/2]).
-export([get/3, get/4, get/5]).
-export([post/6]).

%% ============================================================================
%% User functions
%% ============================================================================

init(C) ->
    Backend = splunkclient_conn:get_backend(C),
    Protocol = splunkclient_conn:get_protocol(C),
    Host = splunkclient_conn:get_host(C),
    Port = splunkclient_conn:get_port(C),
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
    Backend = splunkclient_conn:get_backend(C),
    Request = get_request(get, C, Path, "", Headers, ""),
    send_request(Backend, State, Request).

post(C, State, BasePath, Params, Headers, BodyParams) when is_list(BodyParams) ->
    Type = "application/x-www-form-urlencoded",
    Path = get_path(BasePath, Params),
    Body = list_to_binary(build_query_string(BodyParams)),
    Backend = splunkclient_conn:get_backend(C),
    Request = get_request(post, C, Path, Body, Headers, Type),
    send_request(Backend, State, Request);
post(C, State, BasePath, Params, Headers, Body) when is_binary(Body) ->
    Path = get_path(BasePath, Params),
    Backend = splunkclient_conn:get_backend(C),
    Request = get_request(post, C, Path, Body, Headers, ""),
    send_request(Backend, State, Request).

terminate(C, State) ->
    Backend = splunkclient_conn:get_backend(C),
    ok = Backend:terminate(State).

%% ============================================================================
%% Internal functions
%% ============================================================================

get_request(Method, C, Path, Body, Headers, Type) ->
    #splunkclient_http{method = Method,
                       protocol = splunkclient_conn:get_protocol(C),
                       host = splunkclient_conn:get_host(C),
                       port = splunkclient_conn:get_port(C),
                       path = Path,
                       body = Body,
                       headers = Headers,
                       type = Type}.

get_path(BasePath, Params) ->
    case Params of
        [] ->
            BasePath;
        _Else ->
            build_query_string(Params, BasePath ++ "?")
    end.

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
