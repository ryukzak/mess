#!/bin/sh
erl -make
erl -pa ebin -newshell -env TERM vt100 -name $1@127.0.0.1