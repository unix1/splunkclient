-module(splunkclient_http_httpc).

-behaviour(splunkclient_http_handler).

-export([init/1, terminate/1, send_request/6]).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

init(_Args) ->
    {ok, 0}.

terminate(_State) ->
    ok.

send_request(_State, Method, Uri, "", Headers, "") ->
    HTTPOptions = [{relaxed, true}],
    Options = [],
    {ok, {{_Version, 200, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers}, HTTPOptions, Options),
    {ok, ResponseBody};
send_request(_State, Method, Uri, Body, Headers, Type) ->
    HTTPOptions = [{relaxed, true}],
    Options = [],
    {ok, {{_Version, 200, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers, Type, Body}, HTTPOptions, Options),
    {ok, ResponseBody}.
