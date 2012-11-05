#! /usr/bin/env python
# -*- coding:utf-8 -*-

from collections import namedtuple
import difflib

def msa(ls):
    while len(ls) >= 2:
        (s1, s2), ls = ls[0:2], ls[2:]
        sm = difflib.SequenceMatcher(lambda x: x == None, s1, s2)
        s = []
        for x in sm.get_opcodes():
            if x[0] == 'equal':
                s += s1[x[1]:x[2]]
            else:
                s.append((tuple(s1[x[1]:x[2]]), tuple(s2[x[3]:x[4]])))
        ls = [s] + ls
    return ls

def flatten_each(ls):
    if isinstance(ls, basestring):
        yield ls
    else:
        for x in ls:
            for y in flatten_each(x):
                yield y
        if len(ls) == 0:
            yield ''

def lattice(ls):
    ret = []
    for x in ls:
        s = set()
        for y in flatten_each(x):
            s.add(y)
        ret.append(sorted(list(s)))
    return ret

def flatten(ls):
    ret = []
    while len(ls) >= 2:
        (x, y), ls = ls[0:2], ls[2:]
        if isinstance(x, basestring):
            if isinstance(y, basestring):
                ls = [x + y] + list(ls)
            else:
                ret += [x, flatten(y)]
        else:
            if isinstance(y, basestring):
                ls = [y] + ls
                ret += [flatten(x)]
            else:
                ret += [flatten(x), flatten(y)]

    if len(ls) == 1 and (type(ls[0]) == list or type(ls[0]) == tuple):
        ret += flatten(ls[0])
    elif len(ls) == 1:
        ret += ls
    return ret

def pformat(ls):
    import re
    import pprint
    string = pprint.pformat(flatten(ls))
    return re.sub(r"\\u([0-9a-f]{4})", lambda x: unichr(int("0x"+x.group(1), 16)), string)

if __name__ == '__main__':

    import sys

    print msa([['A','B','C','D','E'],
               ['x','B','C','D','E'],
               ['x','y','B','C','z','E'],
               ['A','B','C','x','y','z','D']])

    print msa(['ABCDE',
               'xBCDE',
               'xyBCzE',
               'ABCxyzD'])

    print pformat(msa([u'ひとりひとりが持つ知識を自由に共有できる世界を、想像してみてください。それが私たちの、誓約なのです。',
                       u'全ての人が自由に全人類の知識の総体を享受できる世界を、想像してみてください。それが私たちの、誓約なのです。',
                       u'人類が自由に知識の総体を共有できる世界を思い浮かべてください。そのことが私たちの約束なのです。',
                       u'あらゆる知識の集積を誰もが自由に利用することのできる世界を、想像してみてください。私たちはそれを実現します。',
                       u'ありとあらゆる知識が集まり、誰でも自由に入手できる世界を、想像してみてください。私たちはそれを実現します。',
                       u'全人類の知の総和を誰もが自由に共有できる世界を、想像してみてください。その実現が、私たちの公約です。']))
