#!/bin/sh
erl -make
erl -pa ebin -newshell -env TERM vt100 -sname $1