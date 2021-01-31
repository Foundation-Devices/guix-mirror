#!/bin/sh

exec >$HOME/xerrors 2>&1

# test: dies a simple X11 program start?
#exec xvt  -geometry  +1+1  -display $DISPLAY

# start plasma under strace and collect trace files in /tmp
/usr/bin/rm -rf /tmp/trace.*
exec /usr/bin/strace -ff -o /tmp/trace.txt -e trace=file \
	    -e trace=file \
	    --no-abbrev -e abbrev=!execve -s 2000 \
     startplasma-x11
#exec startplasma-x11
