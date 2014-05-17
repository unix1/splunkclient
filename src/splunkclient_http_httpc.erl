-module(splunkclient_http_httpc).

-behaviour(splunkclient_http_handler).

-export([init/1, send_request/5]).

init(_) ->
    ok.

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
