PROJECT = splunkclient

# Options

COMPILE_FIRST = splunkclient_http_handler

# Dependencies

DEPS = poolboy gun
dep_poolboy = https://github.com/devinus/poolboy.git 1.2.0
dep_gun = https://github.com/extend/gun.git master

# Standard targets

include erlang.mk
