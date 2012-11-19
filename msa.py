#! /usr/bin/env python
# -*- coding:utf-8 -*-

from collections import namedtuple
from functools import total_ordering
import difflib

def shahash(s_):
    s = ''.join([hex(ord(x)) for x in s_]) if type(s_) == unicode else s_
    import hashlib
    h = hashlib.sha1()
    h.update(s)
    ret = h.hexdigest()
    return ret

@total_ordering
class Sub:
    def __init__(self, parent, pos=None, end=None, seq=None):
        if pos == None:
            self.seq = parent
            self.parent = parent
            self.pos = 0
            self.end = len(parent)
        else:
            self.parent = parent
            self.pos = pos
            self.end = end
            if seq == None:
                self.seq = parent[pos:end]
            else:
                self.seq = seq
    def id(self):
        return 'S_%s_%d_%d' % (shahash(self.parent), self.pos, self.end)
    def __hash__(self):
        return hash(tuple(self.seq))
    def __len__(self):
        return len(self.seq)
    def __getitem__(self, i):
        return self.seq.__getitem__(i)
    def __eq__(self, x):
        return repr(self) == repr(x)
    def __le__(self, x):
        return repr(self) < repr(x)
    def __repr__(self):
        return 's('+repr(self.seq)+')'

@total_ordering
class Disjunct:
    def __init__(self, *args):
        if len(args) == 1:
            if isinstance(args[0], basestring) or isinstance(args[0], Sub):
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
        return self.__class__ == x.__class__ and sorted(self.items) == sorted(x.items)
    def __le__(self, x):
        return repr(self) < repr(x)
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
            if not isinstance(x, basestring) and not isinstance(x, Sub) and len(x) == 1:
                d.add(simplify(x[0]))
            elif isinstance(x, Disjunct):
                for y in x:
                    d.add(simplify(y))
            else:
                d.add(x)
        return Disjunct(list(set(d)))
    def compact(self):
        return Disjunct([compact(x) for x in self.items])

def msa(ls):
    for i in xrange(0, len(ls)):
        ls[i] = [Sub(ls[i], j, j+1) for j in xrange(0, len(ls[i]))]

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

def regions(ls, tp):
    start = None
    length = 0
    rep = []
    for (i,x) in enumerate(ls):
        if isinstance(x, tp):
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
    return rep

def compact(ls):
    if isinstance(ls, Disjunct):
        return ls.compact()
    if not isinstance(ls, list) and not isinstance(ls, tuple):
        return ls
    mask = [True] * len(ls)
    ret = [x for x in ls]
    for (s,l) in regions(ls, basestring):
        ret[s:s+l] = [''.join(ls[s:s+l])] + [None] * (l-1)
        mask[s:s+l] = [False] * l
    for (s,l) in regions(ls, Sub):
        ret[s:s+l] = [Sub(ls[s].parent, ls[s].pos, ls[s+l-1].end)] + [None] * (l-1)
        mask[s:s+l] = [False] * l
    for (i,x) in enumerate(ls):
        if mask[i] and isinstance(ls[i], Disjunct):
            ret[i] = compact(ret[i])
    ret = [x for x in ret if x]
    ret = (ls.__class__)(ret)
    return ret

def simplify(a):
    if isinstance(a, basestring):
        return a
    elif isinstance(a, Disjunct):
        return a.simplify()
    elif isinstance(a, Sub):
        if len(a) == 1 and not isinstance(a.parent, basestring) and isinstance(a[0], Sub):
            return simplify(a[0])
        else:
            return Sub(a.parent, a.pos, a.end, seq=simplify(a.seq))
    else:
        if len(a) == 1:
            return simplify(a[0])
        ret = []
        for x in a:
            x = simplify(x)
            if isinstance(x, basestring) or isinstance(x, Disjunct) or isinstance(x, Sub):
                ret.append(x)
            else:
                ret += x
        return (a.__class__)(ret)

Edge = namedtuple('Edge', 'from_ to fid tid')
def new_edge(f, t):
    if isinstance(f, Sub) and isinstance(t, Sub):
        return Edge(f.seq, t.seq, f.id(), t.id())
    elif isinstance(f, Sub) and isinstance(t, basestring):
        return Edge(f.seq, t, f.id(), t)
    elif isinstance(f, basestring) and isinstance(t, Sub):
        return Edge(f, t.seq, f, t.id())
    elif isinstance(f, basestring) and isinstance(t, basestring):
        return Edge(f, t, f, t)
    else:
        None

def window(iterable, size):
    from itertools import tee, izip
    size = min(size, len(iterable))
    iters = tee(iterable, size)
    for i in xrange(1, size):
        for each in iters[i:]:
            next(each, None)
    return izip(*iters)

def to_edges(align, source=Sub('START'), sink=Sub('END')):
    import inspect
    #print len(inspect.stack()), pformat(repr([align, source, sink]))
    if isinstance(align, Sub) or isinstance(align, basestring):
        if isinstance(source, Sub) or isinstance(source, basestring):
            yield new_edge(source, align)
        if isinstance(sink, Sub) or isinstance(sink, basestring):
            yield new_edge(align, sink)
    elif isinstance(align, Disjunct):
        for x in align:
            for d in to_edges(x, source, sink):
                yield d
    else:
        if len(align) == 0:
            yield Edge(source.seq, sink.seq, source.id(), sink.id())
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
    parser.add_argument('-v', '--verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('-t', '--type', choices=['dot', 'lattice'], default='dot',
                        help='type of output')
    parser.add_argument('-o', '--output', default='-')
    parser.add_argument('-e', '--encoding', default='utf8') 
    parser.add_argument('-m', '--maxlen', type=int, default=30)
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
        print >>sys.stderr, '  ', pformat(((msa(lines))))
        print >>sys.stderr, ' s', pformat((simplify(msa(lines))))
        print >>sys.stderr, 'c ', pformat(compact((msa(lines))))
        print >>sys.stderr, 'cs', pformat(compact(simplify(msa(lines))))

    
    if options.type == 'dot':
        def format_label(s):
            s = s.replace('\\', '\\\\').replace('"', '\\"')
            if len(s) <= options.maxlen:
                return '"%s"' % s
            else:
                a = []
                seg = options.maxlen
                for i in xrange(0, len(s) / seg):
                    a.append(s[i*seg:(i+1)*seg])
                return '<%s>' % '<br/>'.join([x.replace('<', '&lt;').replace('>', '&gt;') for x in a])

        print >>options.output, '''digraph g {
  rankdir = LR;
'''
        for x in to_edges(compact(simplify(msa(lines)))):
            print >>options.output, '  %s[label=%s];' % (x.fid, format_label(x.from_))
            print >>options.output, '  %s[label=%s];' % (x.tid, format_label(x.to))
            print >>options.output, '  %s -> %s; // %s -> %s' % (x.fid, x.tid, x.from_, x.to)
        print >>options.output,'''}
'''
    elif options.type == 'lattice':
        print >>options.output, pformat((msa(lines)))
