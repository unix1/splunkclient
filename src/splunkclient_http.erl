-module(splunkclient_http).
-include("splunkclient.hrl").
-export([get_base_uri/1, get/1, get/2, get/3, post/1, post/2, post/3]).

%% ============================================================================
%% API functions
%% ============================================================================

get_base_uri(C) ->
    Protocol = C#splunkclient_conn.protocol,
    Host = C#splunkclient_conn.host,
    Port = C#splunkclient_conn.port,
    Uri = Protocol ++ "://" ++ Host ++ ":" ++ Port,
    Uri.

get(Uri) ->
    get(Uri, [], []).

get(BaseUri, Params) ->
    get(BaseUri, Params, []).

get(BaseUri, Params, Headers) ->
    case Params of
        [] ->
            Uri = BaseUri;
        _Else ->
            Uri = build_query_string(Params, BaseUri ++ "?")
    end,
    send_request(get, Uri, "", Headers, "").

post(Uri) ->
    post(Uri, [], []).

post(Uri, Params) ->
    post(Uri, Params, []).

post(Uri, Params, Headers) ->
    Type = "application/x-www-form-urlencoded",
    Body = build_query_string(Params),
    send_request(post, Uri, Body, Headers, Type).

%% ============================================================================
%% Internal functions
%% ============================================================================

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

