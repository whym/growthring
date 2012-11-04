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

def flatten(ls):
    ret = []
    while len(ls) >= 2:
        (x, y), ls = list(ls[0:2]), list(ls[2:])
        if type(x) == type(y):
            if type(x) == unicode or type(x) == str:
                ls = [x + y] + ls
                continue
        if type(x) == list or type(x) == tuple:
            x = flatten(x)
        if type(y) == unicode or type(y) == str:
            ret.append(x)
            ls = [y] + ls
            continue
        if type(y) == list or type(y) == tuple:
            y = flatten(y)
        ret += [x, y]
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
