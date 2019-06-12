#!/usr/bin/env python3
#
# GNU Guix --- Functional package management for GNU
# Copyright © 2017,2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
#
# License: GPLv3

"""Filter the guix buildlog for some KDE package to find relevant information.

Main features:
* Print all the output of phases `check` and `configure`.
* For phase `build': filter out warnings we are not interested in

Usage example::
  
  ./kde-filter-buildlog.py /var/log/guix/drvs/ay/…-kconfig-5.49.0.drv.bz2
"""

import sys
import tempfile, bz2
import subprocess

def strip_log(filename):
    if filename.endswith('.bz2'):
        fh = bz2.open(filename, mode="rt")
    else:
        fh = open(filename, mode="rt")
    state = 0
    for l in fh.readlines():
        if not state and l.startswith(("starting phase `check'",
                                       "starting phase `configure'",)):
            state = 1
            yield l.rstrip()
        elif not state and l.startswith("starting phase `build'"):
            state = 2
            yield l.rstrip()
        elif state and l.startswith(("phase `",)):
            if state:
                yield l.rstrip()
            state = 0
        elif state == 2:
            if l.startswith(("About to parse service type file ",)):
                yield  '' ; yield l.rstrip()
            elif l.startswith(("Found property definition ",
                               "Unknown property type for ",
                               'Generated  "')):
                yield l.rstrip()
            else:
                l = l.replace(' -Wl,--fatal-warnings ', ' ')
                if 'warn' in l or 'Warn' in l:
                    if ('[-Wdeprecated-declarations]' in l or
                        '[-Wcpp]' in l or
                        '[-Wswitch]' in l or
                        '[-Wunused-' in l or
                        '[-Wunknown-pragmas]' in l or
                        '[-Wreorder]' in l or
                        '[-Wsign-compare]' in l or                       
                        '[-Wsuggest-override]' in l or
                        '[-Wpedantic]' in l or
                        '[-Wmaybe-uninitialized]' in l or
                        '[-Wextra]' in l or
                        '[-Woverloaded-' in l or
                        '[-Wignored-' in l or
                        '[-Wint-to-pointer-cast]' in l or
                        'Warning: deprecated annotation' in l):
                        continue
                    yield l.rstrip()
        elif state:
            if l.startswith(("-- Could NOT find",)):
                yield  '####' ; yield l.rstrip() ; yield  ''
            else:
                yield l.rstrip()
    fh.close()
    

me = sys.argv[1]
#for l in strip_log(me):
#    print(l)

try:
    # args stolen fron git source, see `man less`
    pager = subprocess.Popen(['less', '-cFRSi'], stdin=subprocess.PIPE)
    has_output = False
    for l in strip_log(me):
        pager.stdin.write(l.encode('utf-8')+b'\n')
        has_output = True
        #print(l.encode('utf-8'), file=pager.stdin)
    #if not has_output:
    #    pager.stdin.write('all lines have been filtered'.encode('utf-8'))
    pager.stdin.close()
    pager.wait()
except KeyboardInterrupt:
    # let less handle this, -K will exit cleanly
    pass
