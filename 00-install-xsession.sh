#!/bin/sh

D=$(dirname "$0")
rm -f $D/xerrors $D/.xerrors ~/xerrors ~/.xerrors
#echo > ~/.xsession "sh -x $(which startkde) >$D/xerrors 2>&1"
cp $D/startkde ~/.xsession
chmod +x ~/.xsession
