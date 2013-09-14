splunkclient
============

Unofficial Splunk SDK Client in Erlang/OTP

Current Status
--------------

This is currently a development PoC. You can log in to any number of configured Splunk instances, issue "oneshot" searches and get back XML responses.

Installation
------------

* compile

      `rebar compile`

Configure
---------

Edit the included `splunkclient.config` file. You can:

* change default parameters such as host name, port, protocol, username, password

* add any number of other hosts to connect to in the `connections` tuple; just make sure each individual connection has a unique tuple name

* note that when calling functions without specifying a connection name, the `splunkclient_conn_default` will be used, so please don't change the name of this default entry

Usage
-----

* start Erlang shell

      `erl -pa ebin/ -config splunkclient.config`

* start splunkclient

      `splunkclient:start().`

* log in to the default configuration host

      `splunkclient:login().`

* alternatively, log in to a named configuration host

      `splunkclient:login(splunkclient_conn_another).`


* run a simple "oneshot" search against the default host

      `splunkclient:oneshot_search("index=main helloworld").`

* alternatively, run a simple "oneshot" search against a named host

      `splunkclient:oneshot_search(splunkclient_conn_another, "index=main helloworld").`

