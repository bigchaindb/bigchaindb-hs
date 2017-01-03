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
    objects = defaultdict(list)
    for l in cmd('ldd', so_in).strip().splitlines():
        if not 'ghc' in l:
            continue
        so, path = re.match('\s+(.*) => ([^\s]*).*', l).groups()
        objects[os.path.dirname(path)].append(so)
    return objects


def do_relocations(so_in, objects):
    out = cmd('chrpath', so_in)
    rpath = out.split('RPATH=')[1].strip()
    rpaths = set(rpath.split(':'))
    for path in list(rpaths):
        for obj in objects.get(path):
            cmd('cp', os.path.join(path, obj), 'libs')
        rpaths.discard(path)
    cmd('chrpath', '-r', '$ORIGIN/libs', so_in)


so = sys.argv[1]
cmd('bash', stdin='mkdir -p build')
os.chdir('build')
cmd('bash', stdin='rm -rf dist-so && mkdir -p dist-so/libs')
os.chdir('dist-so')
cmd('cp', so, '.')
objects = get_objects(so)
so = os.path.basename(so)
do_relocations(so, objects)
print os.path.abspath(so)

