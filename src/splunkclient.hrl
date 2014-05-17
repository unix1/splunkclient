%% state record
-record (splunkclient_conn, {protocol="https", host="localhost", port="8089", user="admin", pass="changeme", token, pool, http_backend}).
