#! /usr/bin/env python
# -*- coding:utf-8 -*-

import unittest
import suffixarray_tf as st

def get_pos(s, q):
    pos = s.find(q)
    return (pos, pos+len(q))

def all_substrings(s):
    return [s[x[0]:x[1]] for x in st.all_subspans(0, len(s))]

class TestSuffixArrayTF(unittest.TestCase):
    def setUp(self):
        None

    def test_lcp(self):
        s = 'ABRACADABRA#'
        self.assertEqual([-1, 0, 1, 4, 1, 1, 0, 3, 0, 0, 0, 2], st.longest_common_prefixes(s, st.suffixes(s)))

    def test_all_subspans(self):
        self.assertEqual(sorted([(0, 1),
                                 (0, 2),
                                 (0, 3),
                                 (1, 2),
                                 (1, 3),
                                 (2, 3)])
                         , sorted(list(st.all_subspans(0, 3))))

    def test_pincer0(self):
        p1 = st.pincer(' ABCDEF', (1,3), [(1,2)])
        p2 = st.pincer('ABCDEF',  (0,2), [(0,1)])
        self.assertEqual(list(p1), list(p2))

    def test_all_subspans0(self):
        p1 = st.all_subspans(1,3)
        p2 = st.all_subspans(0,2)
        self.assertEqual(sorted(list(p1)), sorted([(x[0]+1, x[1]+1) for x in p2]))

    def test_pincer(self):
        s = 'ABCDE'
        q = 'BCD'
        m = 'C'
        self.assertEqual(['BC', 'C', 'BCD', 'CD'], list(st.pincer(s, get_pos(s, q), [get_pos(s, m)])))

    def test_tf(self):
        s = 'mississippi' + st.DELIMIT
        ec = list(st.equivalence_classes(s))
        self.assertEqual(['issi', 'i', 'mississippi', 'p', 's'], [s[x.pos[0]:x.pos[1]] for x in ec if x])

    def test_replace(self):
        s1 = 'TO_BE_OR_NOT_TO_BE' + st.DELIMIT
        s2 = 'TO BE OR NOT TO BE' + st.DELIMIT
        ec1 = list(st.equivalence_classes(s1))
        ec2 = list(st.equivalence_classes(s2))
        def rep(s, x):
            s[x[0]:x[1]].replace('_', ' ')
        self.assertEqual([rep(s2, x.pos) for x in ec2],
                         [rep(s1, x.pos) for x in ec1])

    def test_equiv(self):
        s = 'abrab' + st.DELIMIT
        ec = list(st.equivalence_classes(s))
        ss = set()
        for x in ec:
            for y in list(st.pincer(s, x.pos, x.minimal)):
                if y.find(st.DELIMIT) < 0:
                    ss.add(y)
            #print 'pincer', list(st.pincer(s, x.pos, x.minimal)), x
        self.assertEqual(sorted(set(all_substrings(s[0:len(s)-1]))), sorted(ss))

if __name__ == '__main__':
    unittest.main()
