#!/usr/bin/env python3
#
# GNU Guix --- Functional package management for GNU
# Copyright © 2017,2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
#
# License: GPLv3

import sys
import tempfile, bz2
import subprocess

def find_unknow_properties(filename):
    if filename.endswith('.bz2'):
        fh = bz2.open(filename, mode="rt")
    else:
        fh = open(filename, mode="rt")
    store = []
    for l in fh.readlines():
        if l.startswith(("About to parse service type file ",
                         "Found property definition ",
                         "Unknown property type for ")):
            store.append(l) #.rstrip())
        elif l.startswith('Generated  "'):
            yield l.split('/build/', 1)[1].rstrip(), store
            store = []
    if store:
        yield 'zzzzz', store
    fh.close()

def strip_same_entries(me, there):
    for k in list(me.keys()):
        if k in there and me[k] == there[k]:
            del me[k]
            del there[k]
    

def make_diff(me, there):
    def write_data(data):
       fh = tempfile.NamedTemporaryFile('wt')
       for k in sorted(list(data.keys())):
           print(k, file=fh)
           fh.writelines(data[k])
           print(file=fh) # seperator
       print(file=fh) # enforce newline end end of file
       fh.flush()
       return fh
       
    me_fh = write_data(me)
    there_fh = write_data(there)
    import pdb ; pdb.set_trace()
    #subprocess.call('hexdump %s | tail' % me_fh.name, shell=True)
    #subprocess.call('hexdump %s | tail' % there_fh.name, shell=True)
    subprocess.call(['emacs-diff', me_fh.name, there_fh.name])
    me_fh.close()
    there_fh.close()
    

me, there = sys.argv[1:3]
me = dict(find_unknow_properties(me))
there = dict(find_unknow_properties(there))
strip_same_entries(me, there)
make_diff(me, there)
