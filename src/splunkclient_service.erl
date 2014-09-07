-module(splunkclient_service).

-behavior(httpclient_service_handler).

%% User functions
-export([get_indexes/1]).
-export([get_jobs/1]).
-export([get_saved_searches/1]).
-export([oneshot_search/2]).
-export([send_simple/3]).

%% Behavior callbacks
-export([get_request/3]).

%% ============================================================================
%% User functions
%% ============================================================================

get_indexes(ConnName) ->
    {ok, 200, _, ResponseBody} =
        httpclient_service:request(ConnName, {get_indexes, []}),
    {ok, search_results, ResponseBody}.

get_jobs(ConnName) ->
    {ok, 200, _, ResponseBody} =
        httpclient_service:request(ConnName, {get_jobs, []}),
    {ok, search_results, ResponseBody}.

get_saved_searches(ConnName) ->
    {ok, 200, _, ResponseBody} =
        httpclient_service:request(ConnName, {get_jobs, []}),
    {ok, search_results, ResponseBody}.

oneshot_search(ConnName, SearchTerm) ->
    {ok, 200, _, ResponseBody} =
        httpclient_service:request(ConnName, {oneshot_search, [SearchTerm]}),
    {ok, search_results, ResponseBody}.

send_simple(ConnName, Event, Params) ->
    {ok, 200, _, ResponseBody} =
        httpclient_service:request(ConnName, {send_simple, [Event, Params]}),
    {ok, ResponseBody}.

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

get_request(get_indexes, [], Token) ->
    Headers = [get_auth_header(Token)],
    Path = <<"/services/data/indexes/">>,
    {ok, httpclient_req:new(get, Headers, Path)};
get_request(get_jobs, [], Token) ->
    Headers = [get_auth_header(Token)],
    Path = <<"/services/search/jobs/">>,
    {ok, httpclient_req:new(get, Headers, Path)};
get_request(get_saved_searches, [], Token) ->
    Headers = [get_auth_header(Token)],
    Path = <<"/services/saved/searches/">>,
    {ok, httpclient_req:new(get, Headers, Path)};
get_request(oneshot_search, [SearchTerm], Token) ->
    Headers = [get_auth_header(Token),
               {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    Path = <<"/services/search/jobs/">>,
    Body = [{<<"exec_mode">>, <<"oneshot">>},
            {<<"search">>, iolist_to_binary([<<"search ">>, SearchTerm])}],
    {ok, httpclient_req:new(post, Headers, Path, Body)};
get_request(send_simple, [Event, Params], Token) ->
    Headers = [get_auth_header(Token),
               {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    Path = <<"/services/receivers/simple">>,
    {ok, httpclient_req:new(post, Headers, Path, Params, Event)}.

%% ============================================================================
%% Internal functions
%% ============================================================================

get_auth_header(Token) ->
    {<<"Authorization">>, Token}.
