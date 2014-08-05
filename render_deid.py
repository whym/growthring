#! /usr/bin/env python
# -*- coding:utf-8 -*-

"""Compare deid results (in the format of convert_deid.py output)

First argument is the ground-truth file.
The rest is the system output, where each line may have underscores (_) to denote de-identification (of a character).
"""

import sys
import argparse
import csv
import re

def match_wildcard(s1, s2, wildcard='_'):
    for (i,x) in enumerate(s1):
        if x != wildcard and x != s2[i] and s2[i] != wildcard:
            return False
    return True

def render_text(o, s, wildcard='_'):
    ret = ''
    for (i,x) in enumerate(o):
        if s[i] == wildcard:
            ret += '<em>%s</em>' % x
        else:
            ret += x
    return ret

def render_cols(bcols, others, labeled=True):
    if any(not match_wildcard(bcols[0], x[0]) for x in others):
        raise Exception('inconsistent columns: %s %s' % (repr(bcols), repr(others)))
    bcls = ''
    if labeled and len(bcols) > 1 and bcols[1].strip() != '':
        bcls = 'labeled'
    
    ocls = 'c'
    return '<div class="%s"><div class="c body">%s</div>' % (bcls, bcols[0]) + ''.join('<div class="%s">%s</div>' % (ocls, render_text(bcols[0], x[0])) for x in others) + '</div>'

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('inputs', nargs='+')
    options = parser.parse_args()
    if options.verbose:
        print >>sys.stderr, options

    readers = [[x.strip().split("\t") for x in open(f)] for f in options.inputs]
    base = readers[0]
    readers = readers[1:]
    out = []
    current = []
    n = 1
    sentences = 1
    tags = ['-', '=', '*']
    for (i,bcols) in enumerate(base):
        ss = [reader[i] for reader in readers]
        ret = [bcols[0]]
        out.append('<li>%s</li>' % render_cols(bcols, ss))

    print '''
<style>
.labeled { background: orange; }
.c { display: inline-block; width: 10em; }
em { color: red; }
.body { display: none; }
</style>
<ul>
%s
</ul>
''' % ("\n".join(out))
