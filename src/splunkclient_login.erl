-module(splunkclient_login).

-include_lib("xmerl/include/xmerl.hrl").

-behavior(httpclient_login_handler).

-export([login/2]).

%% ============================================================================
%% Behavior functions
%% ============================================================================

% login(C, HttpState) ->
%     Body = [{"username", httpclient_conn:get_user(C)},
%             {"password", httpclient_conn:get_pass(C)}],
%     {ok, 200, _ResponseHeaders, ResponseBody} = httpclient_http:post(C,
%         HttpState, "/services/auth/login", [], [], Body),
%     {XML, _} = xmerl_scan:string(binary_to_list(ResponseBody)),
%     [#xmlText{value = SessionKey}] =
%         xmerl_xpath:string("/response/sessionKey/text()", XML),
%     {ok, "Splunk " ++ SessionKey}.

login(Conn, HttpState) ->
    Path = <<"/services/auth/login">>,
    Body = [{<<"username">>, httpclient_conn:get_user(Conn)},
            {<<"password">>, httpclient_conn:get_pass(Conn)}],
    Req = httpclient_req:new(post, [], Path, Body),
    {ok, 200, _ResponseHeaders, ResponseBody} =
        httpclient_http:request(Conn, HttpState, Req),
    {XML, _} = xmerl_scan:string(binary_to_list(ResponseBody)),
    [#xmlText{value = SessionKey}] =
        xmerl_xpath:string("/response/sessionKey/text()", XML),
    {ok, iolist_to_binary([<<"Splunk ">>, list_to_binary(SessionKey)])}.

% login(C) ->
%     User = httpclient_conn:get_user(C),
%     Pass = httpclient_conn:get_pass(C),
%     {ok, 200, _, ResponseBody} =
%         httpclient_service:request(Conn, {get_indexes, [User, Pass]}),
%     {XML, _} = xmerl_scan:string(binary_to_list(ResponseBody)),
%     [#xmlText{value = SessionKey}] =
%         xmerl_xpath:string("/response/sessionKey/text()", XML),
%     {ok, "Splunk " ++ SessionKey}.
% 
% get_request(login, [User, Pass], _Token) ->
%     Path = <<"/services/auth/login">>,
%     Body = [{"username", User},
%             {"password", Pass}],
%     {ok, httpclient_req:new(post, Headers, Path, Body)}.
