-module(splunkclient_sup).

-behaviour(supervisor).

%% User functions
-export([start_link/0]).

%% Behavior callbacks
-export([init/1]).

%% ===================================================================
%% User functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Behavior callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
