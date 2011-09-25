#!/bin/sh
SCRIPT_PATH=`dirname $0`
cd $SCRIPT_PATH/..

erl -pa deps/*/ebin ebin \
    -boot start_sasl \
    -s gcprof \
    -K true \
    +P 1000000 \
    +A 8 \
    -name gcprof@$(hostname)