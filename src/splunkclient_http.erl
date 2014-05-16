-module(splunkclient_http).
-include("splunkclient.hrl").
-export([get/2, get/3, get/4, post/2, post/3, post/4]).

%% ============================================================================
%% API functions
%% ============================================================================

get(C, RelUri) ->
    get(C, RelUri, [], []).

get(C, RelUri, Params) ->
    get(C, RelUri, Params, []).

get(C, RelUri, Params, Headers) ->
    BaseUri = get_base_uri(C) ++ RelUri,
    Uri = case Params of
        [] ->
            BaseUri;
        _Else ->
            build_query_string(Params, BaseUri ++ "?")
    end,
    send_request(get, Uri, "", Headers, "").

post(C, RelUri) ->
    post(C, RelUri, [], []).

post(C, RelUri, Params) ->
    post(C, RelUri, Params, []).

post(C, RelUri, Params, Headers) ->
    Uri = get_base_uri(C) ++ RelUri,
    Type = "application/x-www-form-urlencoded",
    Body = build_query_string(Params),
    send_request(post, Uri, Body, Headers, Type).

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

send_request(Method, Uri, "", Headers, "") ->
    HTTPOptions = [{relaxed, true}],
    Options = [],
    {ok, {{_Version, 200, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers}, HTTPOptions, Options),
    {ok, ResponseBody};
send_request(Method, Uri, Body, Headers, Type) ->
    HTTPOptions = [{relaxed, true}],
    Options = [],
    {ok, {{_Version, 200, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers, Type, Body}, HTTPOptions, Options),
    {ok, ResponseBody}.

