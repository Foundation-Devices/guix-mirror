#!/bin/bash

export HOME=$(dirname $(realpath "$0"))
echo $HOME

cd $HOME
../pre-inst-env guix package -p ~/.desktop-profile -m ~/desktop-manifest.scm
if [ $? != 0 ] ; then
   exit
fi

export GUIX_PROFILE=${HOME}/.desktop-profile
#export XDG_CONFIG_DIRS="${GUIX_PROFILE}/etc/xdg"
#export XDG_DATA_DIRS="${GUIX_PROFILE}/share"

# remove remainings from foreign distro
export QT4DOCDIR=
export QT5DOCDIR=
export QTDIR5=
export QTDIR=
export QT_PLUGIN_PATH=
export KDEINIT5_LIBRARY_PATH=
unset QT4DOCDIR QT5DOCDIR QTDIR5 QTDIR QT_PLUGIN_PATH KDEINIT5_LIBRARY_PATH

PATH=
KDEINIT5_LIBRARY_PATH=
QT_PLUGIN_PATH=
source "${GUIX_PROFILE}/etc/profile"

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
