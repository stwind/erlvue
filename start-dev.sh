#!/bin/sh
APP=erlvue
ERL=erl
COOKIE=erlvue
NODE_NAME=$APP@127.0.0.1
CONFIG=priv/app.config
LIBS_DIR="deps"

exec $ERL -pa ebin \
    -boot start_sasl \
    -name $NODE_NAME \
    -setcookie $COOKIE \
    -config $CONFIG \
    -env ERL_LIBS $LIBS_DIR \
    -s lager \
    -s crypto \
    -eval "application:start(xmerl)" \
    -eval "application:start(ranch)" \
    -eval "application:start(cowboy)" \
    -eval "application:start(sockjs)" \
    -s wamp \
    -s $APP \
    -s sync go
