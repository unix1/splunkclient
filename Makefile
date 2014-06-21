PROJECT = splunkclient

# Options

COMPILE_FIRST = splunkclient_http_handler
CT_SUITES = splunk

# Dependencies

DEPS = poolboy gun
dep_poolboy = https://github.com/devinus/poolboy.git 1.2.0
dep_gun = https://github.com/unix1/gun.git fix-body-no-contentlength

# Standard targets

include erlang.mk
