#!/bin/bash

if [ "$1" != "--in-environment" ] ; then

    export HOME=$(realpath $(dirname "$0"))

    exec ../pre-inst-env guix environment -m ~/desktop-manifest.scm \
		    --pure \
		    -- sh $0 --in-environment "$@"
    exit
fi

# in environment
shift # remove --in-environment

#export QT_LOGGING_RULES="*.debug=true"

#export XDG_RUNTIME_DIR=$HOME/.run
#mkdir -p "$XDG_RUNTIME_DIR"
#chmod 0700 "$XDG_RUNTIME_DIR"

echo PATH=$PATH
echo XDG_DATA_DIRS=$XDG_DATA_DIRS
echo XDG_CONFIG_DIRS=$XDG_CONFIG_DIRS
echo QT_PLUGIN_PATH=$QT_PLUGIN_PATH
echo KDEINIT5_LIBRARY_PATH=$KDEINIT5_LIBRARY_PATH
echo QML2_IMPORT_PATH=$QML2_IMPORT_PATH

# DISPLAY must be *first* argument to xinit after `--`, otherwise
# xinit will assume display :0
# Use the foreign system's xinit
#  This will start the  X server as defined in .xserverrc and
#  connect the clients defined in .xinitrc to it.
#  Use these scripts (instead of passing commands here) to be more flexible
#  in adjusting environments
/usr/bin/xinit --  :1

#pkill -9 kwin_x11 baloo
