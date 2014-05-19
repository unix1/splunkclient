-module(splunkclient_http_gun).
-include("splunkclient.hrl").
-behaviour(splunkclient_http_handler).
-export([init/3, terminate/1, send_request/2]).

-record (state, {pid}).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

init(Protocol, Host, Port) ->
    Type = if Protocol =:= "https" -> ssl; true -> tcp end,
    {ok, Pid} = gun:open(Host, Port, [{type, Type}]),
    _Mref = monitor(process, Pid),
    S = #state{pid = Pid},
    {ok, S}.

send_request(#state{pid = Pid}, #splunkclient_http{method = get,
                                                   protocol = _Protocol,
                                                   host = _Host,
                                                   port = _Port,
                                                   path = Path,
                                                   body = "",
                                                   headers = Headers,
                                                   type = ""}) ->
    StreamRef = gun:get(Pid, Path, Headers),
    {ok, _Status, _ResponseHeaders, _ResponseBody} = receive_data(Pid, StreamRef);
send_request(#state{pid = Pid}, #splunkclient_http{method = Method,
                                                   protocol = _Protocol,
                                                   host = _Host,
                                                   port = _Port,
                                                   path = Path,
                                                   body = Body,
                                                   headers = Headers0,
                                                   type = Type}) ->
    Headers = [{"content-type", Type} | Headers0],
    StreamRef = gun:Method(Pid, Path, Headers, Body),
    {ok, _Status, _ResponseHeaders, _ResponseBody} = receive_data(Pid, StreamRef).

receive_data(Pid, StreamRef) ->
    receive
        {'DOWN', _Tag, _, _, Reason} ->
            % TODO remove error logger
            io:format("gun process disconnected with reason: ~s~n", [Reason]),
            exit(Reason);
        {gun_response, Pid, StreamRef, fin, Status, Headers} ->
            {ok, Status, Headers, <<>>};
        {gun_response, Pid, StreamRef, nofin, Status, Headers} ->
            {ok, ResponseBody} = receive_data_loop(Pid, StreamRef, <<>>),
            {ok, Status, Headers, ResponseBody}
    after 1000 ->
        exit(timeout)
    end.

receive_data_loop(Pid, StreamRef, Acc) ->
    receive
        {'DOWN', _Tag, _, _, Reason} ->
            io:format("error receiving (incomplete) with reason: ~s~n", [Reason]),
            {error, incomplete};
        {gun_data, Pid, StreamRef, nofin, Data} ->
            io:format("~s~n", [Data]),
            receive_data_loop(Pid, StreamRef, [Acc, Data]);
        {gun_data, Pid, StreamRef, fin, Data} ->
            io:format("~s~n", [Data]),
            {ok, iolist_to_binary([Acc, Data])}
    after 1000 ->
        {error, timeout}
    end.

terminate(S) ->
    gun:shutdown(S#state.pid),
    ok.
