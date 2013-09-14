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

get(Uri, Params) ->
    get(Uri, Params, []).

get(Uri, Params, Headers) ->
    % TODO
    {Uri, Params, Headers}.

post(Uri) ->
    post(Uri, [], []).

post(Uri, Params) ->
    post(Uri, Params, []).

post(Uri, Params, Headers) ->
    Method = post,
    Type = "application/x-www-form-urlencoded",
    Body = build_query_string(Params),
    HTTPOptions = [{relaxed, true}],
    Options = [],
    {ok, {{_Version, 200, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers, Type, Body}, HTTPOptions, Options),
    {ok, ResponseBody}.

build_query_string(Params) ->
    build_query_string(Params, "").

build_query_string([], Acc) ->
    Acc;
build_query_string([{Name, Value}|Rest], Acc) ->
    build_query_string(
        Rest,
        Acc ++ "&" ++ http_uri:encode(Name) ++ "=" ++ http_uri:encode(Value)
    ).
