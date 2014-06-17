splunkclient
============

Unofficial Splunk SDK Client in Erlang/OTP

Current Status
--------------

This is currently a development PoC. You can log in to any number of configured Splunk instances, issue "oneshot" searches and get back XML responses.

Configure
---------

Optionally edit the included `splunkclient.config` file. You can:

* change default parameters such as host name, port, protocol, username, password

* add any number of other hosts to connect to in the `connections` tuple; just make sure each individual connection has a unique tuple name

* note that when calling functions without specifying a connection name, the `splunkclient_conn_default` will be used, so please don't change the name of this default entry

* change default pool and worker configuration

Installation
------------

* compile

      `make`

Usage
-----

* start with Erlang console

      `./_rel/bin/splunkclient_release-1.0.0 console`

* log in to the default connection host, or a specified named connection

      `splunkclient:login().`

      `splunkclient:login(splunkclient_conn_another).`

* run a simple "oneshot" search against the default or a specified named connection

      `splunkclient:oneshot_search("index=main helloworld").`

      `splunkclient:oneshot_search(splunkclient_conn_another, "index=main helloworld").`

* send an event

      `splunkclient:send_simple(<<"2014-06-16 09:19:43 Tester,Record">>, [{"host","localhost"},{"source","testing1"},{"sourcetype","test-type"}]).`

* other useful functions

      `splunkclient:get_indexes().`

      `splunkclient:get_jobs().`

      `splunkclient:get_saved_searches().`
