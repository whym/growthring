#! /usr/bin/env python
# -*- coding:utf-8 -*-

from collections import namedtuple
import difflib

class Disjunct:
    def __init__(self, *args):
        if len(args) == 1:
            if isinstance(args[0], basestring):
                self.items = [args[0]]
            else:
                self.items = args[0]
        elif len(args) > 1:
            self.items = args
        else:
            self.items = []
    def add(self, x):
        self.items.append(x)
    def __iter__(self):
        return self.items.__iter__()
    def __hash__(self):
        return tuple(sorted(self.items)).__hash__()
    def __len__(self):
        return self.items.__len__()
    def __eq__(self, x):
        return type(self) == type(x) and sorted(self.items) == sorted(x.items)
    def __getitem__(self, i):
        return self.items.__getitem__(i)
    def __repr__(self):
        return 'd'+tuple(self.items).__repr__()
    def simplify(self):
        if len(self) == 1:
            return simplify(self.items[0])
        d = Disjunct([])
        for x in self.items:
            x = simplify(x)
            if not isinstance(x, basestring) and len(x) == 1:
                d.add(simplify(x[0]))
            elif isinstance(x, Disjunct):
                for y in x:
                    d.add(simplify(y))
            else:
                d.add(x)
        return Disjunct(list(set(d)))
    def compact(self):
        return Disjunct([compact(x) for x in self.items])

class Seq:
    def __init__(self, items=[]):
        self.items = items
    def append(self, x):
        self.items.append(x)
    def lappend(self, x):
        self.items += x
    def lprepend(self, x):
        self.items = x + self.items
    def __iter__(self):
        return self.items.__iter__()

def msa(ls):
    while len(ls) >= 2:
        (s1, s2), ls = ls[0:2], ls[2:]
        sm = difflib.SequenceMatcher(lambda x: x == None, s1, s2)
        s = []
        for x in sm.get_opcodes():
            if x[0] == 'equal':
                s += s1[x[1]:x[2]]
            else:
                s.append(Disjunct(tuple(s1[x[1]:x[2]]), tuple(s2[x[3]:x[4]])))
        ls = [s] + ls
    if isinstance(ls, list) and len(ls) == 1 and isinstance(ls[0], list):
        ls = ls[0]
    return ls

def compact(ls):
    if isinstance(ls, Disjunct):
        return ls.compact()
    if not isinstance(ls, list) and not isinstance(ls, tuple):
        return ls
    start = None
    length = 0
    rep = []
    for (i,x) in enumerate(ls):
        if isinstance(x, basestring):
            if start == None:
                start = i
            length += 1
        else:
            if start != None:
                rep.append((start, length))
                start = None
                length = 0
    if start != None:
        rep.append((start, length))
    mask = [True] * len(ls)
    ret = [x for x in ls]
    for (s,l) in rep:
        ret[s:s+l] = [''.join(ls[s:s+l])] + [None] * (l-1)
        mask[s:s+l] = [False] * l
    for (i,x) in enumerate(ls):
        if mask[i] and isinstance(ls[i], Disjunct):
            ret[i] = compact(ret[i])
    ret = [x for x in ret if x]
    if isinstance(ls, tuple):
        ret = tuple(ret)
    return ret

def simplify(a):
    if isinstance(a, basestring):
        return a
    elif isinstance(a, Disjunct):
        return a.simplify()
    else:
        if len(a) == 1:
            return simplify(a[0])
        ret = []
        for x in a:
            x = simplify(x)
            if isinstance(x, basestring):
                ret.append(x)
            elif isinstance(x, Disjunct):
                ret.append(x)
            else:
                ret += x
        return (type(a))(ret)

Edge = namedtuple('Edge', 'from_ to')

def window(iterable, size):
    from itertools import tee, izip
    size = min(size, len(iterable))
    iters = tee(iterable, size)
    for i in xrange(1, size):
        for each in iters[i:]:
            next(each, None)
    return izip(*iters)

def to_edges(align, source='START', sink='END'):
    import inspect
    #print len(inspect.stack()), pformat(repr([align, source, sink]))
    if isinstance(align, basestring):
        if isinstance(source, basestring):
            yield Edge(source, align)
        ## ↓があると重複する？
        # else:
        #     for e in to_edges(source, source='', sink=align):
        #         if e.to == align:
        #             yield Edge(e.from_, align)
        if isinstance(sink, basestring):
            yield Edge(align, sink)
        ## ↓があると重複する？
        # else:
        #     for e in to_edges(sink, align, sink=''):
        #         if e.from_ == align:
        #             yield Edge(align, e.to)
    elif isinstance(align, Disjunct):
        for x in align:
            for d in to_edges(x, source, sink):
                yield d
    else:
        if len(align) == 0:
            yield Edge(source, sink)
        else:
            for (x,y,z) in window([source]+list(align)+[sink], 3):
                for d in to_edges(y, x, z):
                    yield d

def pformat(ls):
    import re
    import pprint
    string = pprint.pformat(ls)
    return re.sub(r"\\u([0-9a-f]{4})", lambda x: unichr(int("0x"+x.group(1), 16)), string)

if __name__ == '__main__':

    import sys
    import codecs

    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--verbose',
                        dest='verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('-o', '--output', default='-')
    parser.add_argument('-e', '--encoding', default='utf8') 
    parser.add_argument('inputs', nargs='+')
    options = parser.parse_args()

    if options.output == '-':
        options.output = codecs.getwriter(options.encoding)(sys.stdout)
    else:
        options.output = codecs.open(options.output, 'w', encoding=options.encoding)
    if options.inputs == ['-']:
        options.inputs = [codecs.getreader(options.encoding)(sys.stdin)]
    else:
        options.inputs = [codecs.open(x, encoding=options.encoding) for x in options.inputs]
    sys.stderr = codecs.getwriter(options.encoding)(sys.stderr)

    lines = []
    for f in options.inputs:
        lines += [x.strip() for x in f.readlines()]

    if options.verbose:
        print >>sys.stderr, pformat(compact(simplify(msa(lines))))

    print >>options.output, '''digraph g {
  rankdir = LR;
'''
    for x in to_edges(compact(simplify(msa(lines)))):
        print >>options.output, '  %s -> %s;' % (x.from_, x.to)
    print >>options.output,'''}
'''
