#!/usr/bin/env python

import os
import os.path
import subprocess
from collections import defaultdict
import sys
import re


def cmd(*args, **kwargs):
    stdin = kwargs.get('stdin')
    p = subprocess.Popen(args, stdout=subprocess.PIPE,
                               stdin=stdin and subprocess.PIPE)
    if stdin:
        p.stdin.write(stdin)
        p.stdin.close()
    assert p.wait() == 0
    return p.stdout.read()


def get_objects(so_in):
    """
    Returns a mapping of paths -> objects that will be loaded
    """
    objects = defaultdict(list)
    for l in cmd('ldd', so_in).strip().splitlines():
        if not 'ghc' in l:
            continue
        so, path = re.match('\s+(.*) => ([^\s]*).*', l).groups()
        objects[os.path.dirname(path)].append(so)
    return dict(objects)


def do_relocations(so_in, objects):
    out = cmd('chrpath', so_in)
    rpath = out.split('RPATH=')[1].strip()
    rpaths = set(rpath.split(':'))
    # Why even bother to iterate the paths? Why not just copy
    # the objects direct from the ldd list and then update RPATH...
    for path in list(rpaths):
        objs = objects.get(path)
        if not objs:
            print >>sys.stderr, "No objects for path:", path 
        else:
            for obj in objs:
                cmd('cp', os.path.join(path, obj), 'libs')
        rpaths.discard(path)
    cmd('chrpath', '-r', '$ORIGIN/libs', so_in)


so = os.path.abspath(sys.argv[1])
cmd('bash', stdin='mkdir -p build')
os.chdir('build')
cmd('bash', stdin='rm -rf dist-so && mkdir -p dist-so/libs')
os.chdir('dist-so')
cmd('cp', so, '.')
objects = get_objects(so)
so = os.path.basename(so)
do_relocations(so, objects)
print os.path.abspath(so)

