
This directory contains file to test the desktop following the idea from
<https://guix.gnu.org/de/blog/2019/running-a-guix-xfce-desktop-on-centos-7/>.
The basic idea: Start an X11 server (provided by the host OS) on another
virtual terminal and make this run the desktop.

See comments in desktop-manifest.scm for which packages get installed and what
combinations I already treid.



Brief howto
=================

* Switch to a new virtual terminal (e.g Ctrl-Alt-F2) and login there
  (maybe this is not necessary, but I was not able to start a new X11 server
  from within my running X session)

* cd here
* Adjust desktop-manifest.scm
* Adjust .xinitrc

* Run ./00-setup.sh

  This script will set this directory as your HOME, create a Guix profile
  containing the Plasma desktop and start the X11 server with the Plasma
  desktop on the next virtaul terminal.

* check results, refine packages, adjust desktop-manifest.scm redo :-)

* new X-server will get DISPLAY :1.
  To change this, change number at the end of 00-setup.sh  and 01-setup.sh


Files
============

00-setup.sh  - starts using a profile - might require tweaking
01-setup.sh  - starts using an environment - unfinished
desktop-manifest.scm - packages to install.
             Also contains information about what I already tried.

.xserverrc  - starts X server on next vt - called by xinit
.xinitrc    - start the X clients, esp. the desktop
              Use these scripts to be more flexible in adjusting environments

xorg.conf  - unused in the Plase desktop test playground. *-setup.sh uses the
             host OS's xinit and the host OS's xorg.conf

unstrace.py  - unfinished helper to strip irrelevant information from
 	       the strace logs



