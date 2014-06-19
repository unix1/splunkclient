%% http request record
-record (splunkclient_http, {method, protocol, host, port, path, body, headers, type}).
