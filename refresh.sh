#!/usr/bin/env bash

NAME=$(git log -p -1 HEAD | grep -F '(name "' | tail -1 | sed 's/.*"\(.*\)".*/\1/')

echo name: $NAME
./pre-inst-env guix refresh -u $NAME

git add gnu/packages/kde-plasma.scm
git commit --amend --no-edit '-S'
