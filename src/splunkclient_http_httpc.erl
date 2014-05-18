-module(splunkclient_http_httpc).
-include("splunkclient.hrl").
-behaviour(splunkclient_http_handler).
-export([init/3, terminate/1, send_request/2]).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

init(_Protocol, _Host, _Port) ->
    {ok, 0}.

send_request(_State, #splunkclient_http{method = Method,
                                        protocol = Protocol,
                                        host = Host,
                                        port = Port,
                                        path = Path,
                                        body = "",
                                        headers = Headers,
                                        type = ""}) ->
    HTTPOptions = [{relaxed, true}],
    Options = [],
    Uri = get_uri(Protocol, Host, Port, Path),
    {ok, {{_Version, 200, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers}, HTTPOptions, Options),
    {ok, ResponseBody};
send_request(_State, #splunkclient_http{method = Method,
                                        protocol = Protocol,
                                        host = Host,
                                        port = Port,
                                        path = Path,
                                        body = Body,
                                        headers = Headers,
                                        type = Type}) ->
    HTTPOptions = [{relaxed, true}],
    Options = [],
    Uri = get_uri(Protocol, Host, Port, Path),
    {ok, {{_Version, 200, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers, Type, Body}, HTTPOptions, Options),
    {ok, ResponseBody}.

terminate(_State) ->
    ok.

%% ============================================================================
%% Internal functions
%% ============================================================================

get_uri(Protocol, Host, Port, Path) ->
    Protocol ++ "://" ++ Host ++ ":" ++ Port ++ Path.
