-module(splunkclient_http_httpc).

-include("splunkclient.hrl").

-behaviour(splunkclient_http_handler).

-export([init/3]).
-export([terminate/1]).
-export([send_request/2]).

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
    Options = [{body_format, binary}],
    Uri = get_uri(Protocol, Host, Port, Path),
    {ok, {{_Version, Status, _ReasonPhrase}, ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers}, HTTPOptions, Options),
    {ok, Status, ResponseHeaders, ResponseBody};
send_request(_State, #splunkclient_http{method = Method,
                                        protocol = Protocol,
                                        host = Host,
                                        port = Port,
                                        path = Path,
                                        body = Body,
                                        headers = Headers,
                                        type = Type}) ->
    HTTPOptions = [{relaxed, true}],
    Options = [{body_format, binary}],
    Uri = get_uri(Protocol, Host, Port, Path),
    {ok, {{_Version, Status, _ReasonPhrase}, ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers, Type, Body}, HTTPOptions, Options),
    {ok, Status, ResponseHeaders, ResponseBody}.

terminate(_State) ->
    ok.

%% ============================================================================
%% Internal functions
%% ============================================================================

get_uri(Protocol, Host, Port, Path) ->
    Protocol ++ "://" ++ Host ++ ":" ++ integer_to_list(Port) ++ Path.
