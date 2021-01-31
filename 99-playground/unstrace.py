#!/usr/bin/env python3

import re, os

def split_args(args):
    a = args.split('"', 1)[1]
    a = a.split('"')[0]
    return a


cmd_map = {
    "statx": "stat",
    "lstat": "stat",
    "openat": "open",
    "access": "stat",
}

seen = set()

def stage1(cmds):
    last_cmd, last_path, last_rc = None, None, None
    for cmd, path, rc in cmds:
        if (cmd, path, rc) in seen:
            continue
        seen.add((cmd, path, rc))
        if (last_cmd == "open" and last_rc == False and
            cmd == "stat" and rc == False and
            os.path.dirname(last_path) == path):
            # stat on dir after trying to access file
            last_path = path
            continue
        if rc == last_rc:
            # unchanged status
            if path == last_path:
                last_cmd, last_path, last_rc = cmd, path, rc
            elif path.startswith(path):
                last_path = path
                last_cmd, last_path, last_rc = cmd, path, rc
            else:
                last_cmd, last_path, last_rc = cmd, path, rc
                yield(cmd, path, rc)
        else:
            last_cmd, last_path, last_rc = cmd, path, rc
            yield(cmd, path, rc)

LIB_SEARCH = 1
PLUGIN_SEARCH = 2

def stage2(cmds):
    last_cmd, last_path, last_rc = None, None, None
    state = None
    for cmd, path, rc in cmds:
        #breakpoint()
        if (state == LIB_SEARCH):
            if rc == True:
                # found
                searched_dirs = []
                yield cmd, path, rc
                continue
            elif os.path.basename(path) == last_path:
                # still seeking
                searched_dirs.append(os.path.dirname(path))
                continue
            else:
                #breakpoint()
                # next library
                for p in searched_dirs:
                    yield cmd, last_path + " " + p, False
                searched_dirs = []

        if (cmd == "open" and rc == False and
            re.search(r'\.so(\.[0-9]+)?', path)):
            # library, access failed here
            state = LIB_SEARCH
            searched_dirs = [os.path.dirname(path)]
            last_path = os.path.basename(path)
        else:
            yield cmd, path, rc


def strip_store_names(cmds):
    import pathlib
    HOME = str(pathlib.Path.home())
    PWD = str(pathlib.Path.cwd())
    pwd_first = len(PWD) > len(HOME)
    for cmd, path, result in cmds:
        if path.startswith("/gnu/store/"):
            path = "/gnu/store/…" + path[43:]
        elif pwd_first and path.startswith(PWD):
            path = "$PWD" + path[len(PWD):]
        elif path.startswith(HOME):
            path = "~" + path[len(HOME):]
        elif path.startswith(PWD):
            path = "$PWD" + path[len(PWD):]
        yield cmd, path, result


def main(filename):
    with open(filename) as fh:
        lines = fh.readlines()
    matches = (re.match(r"^(\w+)\(([^)]+)\) = (\S+).*", l)
               for l in lines)
    cmds = (m.groups() for m in matches if m)
    cmds = ((cmd_map.get(cmd, cmd), split_args(path), int(result) >= 0)
            for cmd, path, result in cmds
            if not "/share/locale/" in path)
    cmds = strip_store_names(cmds)
    cmds = stage1(cmds)
    cmds = stage2(cmds)
    for cmd, path, rc in cmds:
        print(cmd, path, "" if rc else "***")



import argparse
parser = argparse.ArgumentParser()
parser.add_argument("filename")
args = parser.parse_args()
main(args.filename)
