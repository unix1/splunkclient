-module(splunkclient_service_event).

-export([handle_call/2, handle_info/2, code_change/3]).
-export([init/1, terminate/2, handle_event/2]).

-behaviour(gen_event).

%% ============================================================================
%% Behaviour callbacks
%% ============================================================================

%% gen_event init, set state to given gen_server Pid
init([Pid]) ->
    {ok, Pid}.

% handle a gen_event callback for token update, state is gen_server Pid
handle_event({token, Token}, S) ->
    splunkclient_service:update_connection_token(S, Token),
    {ok, S}.

handle_call(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

terminate([], _State) ->
    ok.
