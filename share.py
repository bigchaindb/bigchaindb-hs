import ujson as json
import ctypes
from ctypes import *
import glob
import os.path
import time
import sys


so = max(glob.glob('.stack-work/install/x86_64-linux/lts-7.14/8.0.1/lib/x86_64-linux-ghc-8.0.1/bigchaindb-hs*/libHSbigchaindb*.so'), key=os.path.getctime)
so = os.environ.get('SO', so)
print >>sys.stderr, so
bdb=cdll.LoadLibrary(so)

FTYPE = CFUNCTYPE(None, c_char_p)
_res = [None]

def cb(data):
    _res[0] = json.loads(data)

def call(name, req):
    getattr(bdb, name)(req, FTYPE(cb))
    return _res[0]

import sys, pprint, os
repeat = int(os.environ.get('REPEAT', '1'))
start = time.time()
for i in xrange(repeat):
    out = call(sys.argv[1], sys.argv[2])
print json.dumps(out, indent=2)
print >>sys.stderr, "%.3f" % (time.time() - start)
