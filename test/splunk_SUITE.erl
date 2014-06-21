-module(splunk_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([login/1]).
-export([login_named/1]).
-export([get_indexes/1]).
-export([get_indexes_named/1]).
-export([get_saved_searches/1]).
-export([get_saved_searches_named/1]).
-export([get_jobs/1]).
-export([get_jobs_named/1]).
-export([oneshot_search/1]).
-export([oneshot_search_named/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [login,
     login_named,
     get_indexes,
     get_indexes_named,
     get_saved_searches,
     get_saved_searches_named,
     get_jobs,
     get_jobs_named,
     oneshot_search,
     oneshot_search_named].

init_per_suite(Config) ->
    ok = application:load(splunkclient),
    [application:set_env(App, Key, Val) || {App, Key, Val} <-
     [{splunkclient, connections,
       [{default,
         [{protocol, "https"},
          {host, "localhost"},
          {port, 8089},
          {user, "admin"},
          {pass, "changeme"},
          {pool, splunkclient_service_default},
          {http_backend, splunkclient_http_gun}]}]}, % backend for login service
      {splunkclient, service_pools,
       [{splunkclient_service_default,
         [ % size args
           {size, 10}, % max pool size
           {max_overflow, 0}], % max # of workers created if pool is empty
         [ % worker args
           {connection, default},
           {http_backend, splunkclient_http_gun}]}] % backend for service workers
       }]
    ],
    ok = splunkclient:start(),
    Config.

end_per_suite(_) ->
    ok = splunkclient:stop(),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

login(_) ->
    ok = splunkclient:login().

login_named(_) ->
    ok = splunkclient:login(default).

get_indexes(_) ->
    {ok, search_results, _} = splunkclient:get_indexes().

get_indexes_named(_) ->
    {ok, search_results, _} = splunkclient:get_indexes(default).

get_saved_searches(_) ->
    {ok, search_results, _} = splunkclient:get_saved_searches().

get_saved_searches_named(_) ->
    {ok, search_results, _} = splunkclient:get_saved_searches(default).

get_jobs(_) ->
    {ok, search_results, _} = splunkclient:get_jobs().

get_jobs_named(_) ->
    {ok, search_results, _} = splunkclient:get_jobs(default).

oneshot_search(_) ->
    {ok, search_results, _} = splunkclient:oneshot_search("xyz earliest=-1m").

oneshot_search_named(_) ->
    {ok, search_results, _} = splunkclient:oneshot_search(default,
        "xyz earliest=-1m").
