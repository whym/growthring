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
        d = Disjunct([])
        for x in self.items:
            if isinstance(x, basestring):
                d.add(x)
            elif isinstance(x, Disjunct):
                for y in x.simplify():
                    d.add(y)
            elif isinstance(x, tuple) and len(x) == 1:
                if isinstance(x[0], Disjunct):
                    d.add(x[0].simplify())
                else:
                    t = type(x[0])
                    d.add(t([y for y in simplify(x[0])]))
            elif isinstance(x, tuple):
                d.add(tuple([y.simplify() if isinstance(y, Disjunct) else y for y in x]))
            else:
                d.add(x)
        return d
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
        yield a
    if (isinstance(a, list) or isinstance(a, tuple)) and len(a) == 1:
        for x in simplify(a[0]):
            yield x
    else:
        for x in a:
            if isinstance(x, Disjunct):
                yield x.simplify()
            else:
                yield x
            
def pformat(ls):
    import re
    import pprint
    string = pprint.pformat(ls)
    return re.sub(r"\\u([0-9a-f]{4})", lambda x: unichr(int("0x"+x.group(1), 16)), string)

if __name__ == '__main__':

    import sys

    for x in simplify(msa([['A','B','C','D','E'],
                           ['x','B','C','D','E'],
                           ['x','y','B','C','z','E'],
                           ['A','B','C','x','y','z','D']])):
        print x

    print [x for x in simplify(msa(['ABCDE',
                                    'xBCDE',
                                    'xyBCzE',
                                    'ABCxyzD']))]

    print pformat(compact([x for x in simplify(msa([u'ひとりひとりが持つ知識を自由に共有できる世界を、想像してみてください。それが私たちの、誓約なのです。',
                       u'全ての人が自由に全人類の知識の総体を享受できる世界を、想像してみてください。それが私たちの、誓約なのです。',
                       u'人類が自由に知識の総体を共有できる世界を思い浮かべてください。そのことが私たちの約束なのです。',
                       u'あらゆる知識の集積を誰もが自由に利用することのできる世界を、想像してみてください。私たちはそれを実現します。',
                       u'ありとあらゆる知識が集まり、誰でも自由に入手できる世界を、想像してみてください。私たちはそれを実現します。',
                       u'全人類の知の総和を誰もが自由に共有できる世界を、想像してみてください。その実現が、私たちの公約です。']))]))
