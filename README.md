splunkclient
============

Unofficial Splunk SDK Client in Erlang/OTP

Current Status
--------------

This is currently a development PoC. You can log in to any number of configured Splunk instances, send events, issue "oneshot" searches and get back XML responses.

Configure
---------

Optionally edit the included `splunkclient.config` file. You can:

* change connection parameters such as host name, port, protocol, username, password

* add any number of other hosts to connect to in the `connections` tuple; just make sure each individual connection has a unique tuple name

* change default pool configuration, e.g. number of workers available for each connection pool, HTTP client backend, etc.

Installation
------------

* to use it in your application, include it as a dependency

* to play with it in the console, you can compile

      `make`

* you can run tests (requires locally installed Splunk with default configuration)

      `make tests`

Usage
-----

To start in Erlang console

```
_rel/bin/splunkclient_release console
```

Log in to the default connection host, or a specified named connection

```erlang
splunkclient:login().

splunkclient:login(splunkclient_conn_another).
```

Run a simple "oneshot" search against the default or a specified named connection

```erlang
splunkclient:oneshot_search("index=main helloworld").

splunkclient:oneshot_search(splunkclient_conn_another, "index=main helloworld").
```

Send an event

```erlang
splunkclient:send_simple(<<"2014-06-16 09:19:43 Tester,Record">>, [{"host","localhost"},{"source","testing1"},{"sourcetype","test-type"}]).
```

Other useful functions

```erlang
splunkclient:get_indexes().
splunkclient:get_jobs().
splunkclient:get_saved_searches().
```

Note that when calling functions without specifying a connection name, the `default` will be used, so please don't change the name of this default entry in configuration
