#!/bin/sh

pids=$(pgrep X);
[ -n "$pids" ] && kill $pids
