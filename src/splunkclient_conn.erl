-module(splunkclient_conn).

%% connection record
-record (splunkclient_conn, {protocol="https", host="localhost", port="8089",
            user="admin", pass="changeme", token, pool, http_backend}).

%% User functions
-export([new/7]).
-export([get_backend/1]).
-export([get_host/1]).
-export([get_pass/1]).
-export([get_port/1]).
-export([get_protocol/1]).
-export([get_token/1]).
-export([get_user/1]).
-export([set_backend/2]).
-export([set_token/2]).

%% ============================================================================
%% User functions
%% ============================================================================

new(Protocol, Host, Port, User, Pass, Pool, HttpBackend) ->
    #splunkclient_conn{protocol = Protocol, host = Host, port = Port,
        user = User, pass = Pass, pool = Pool, http_backend = HttpBackend}.

get_backend(Conn) ->
    Conn#splunkclient_conn.http_backend.

get_host(Conn) ->
    Conn#splunkclient_conn.host.

get_pass(Conn) ->
    Conn#splunkclient_conn.pass.

get_port(Conn) ->
    Conn#splunkclient_conn.port.

get_protocol(Conn) ->
    Conn#splunkclient_conn.protocol.

get_token(Conn) ->
    Conn#splunkclient_conn.token.

get_user(Conn) ->
    Conn#splunkclient_conn.user.

set_backend(Conn, Backend) ->
    Conn#splunkclient_conn{http_backend = Backend}.

set_token(Conn, Token) ->
    Conn#splunkclient_conn{token = Token}.
