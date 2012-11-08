#! /usr/bin/env python

import unittest
import msa

class TestDisjunct(unittest.TestCase):
    def setUp(self):
        None

    def test_disjunct_simplify_flat(self):
        from msa import Disjunct as d
        self.assertEqual(d(['1','2','4','3']),
                         d(['1',d('2','4'),'3']).simplify())
        self.assertEqual(d(['1','2','4','','3']),
                         d(['1',d('2',d('4','')),'3']).simplify())
    def test_disjunct_simplify_nonflat(self):
        from msa import Disjunct as d
        self.assertEqual(d(['1','2',('4','5'),'','3']),
                         d(['1',d('2',d(('4','5'),'')),'3']).simplify())
        self.assertEqual(d(['1','2',('4','5'),'','3']),
                         d(['1',d('2',d(('4',('5',)),'')),'3']).simplify())
    def test_disjunct_simplify_empty(self):
        from msa import simplify, Disjunct as d
        self.assertEqual(['1', d('2', ()), '3'],
                         simplify(['1', d('2',()), '3']))
    def test_disjunct_compact(self):
        from msa import Disjunct as d
        self.assertEqual(d(['1',d('2',d(('45',  ),'')),'3']),
                         d(['1',d('2',d(('4','5'),'')),'3']).compact())

    def test_disjunct_compact_simplify(self):
        from msa import Disjunct as d
        self.assertEqual(d(['1','2',('45',) ,'','3']),
                         d(['1',d('2',d(('4','5'),'')),'3']).simplify().compact())

class TestMSA(unittest.TestCase):
    def setUp(self):
        None

    def test_str_list(self):
        from msa import msa, simplify
        self.assertEqual(simplify(msa([['A','B','C','D','E'],
                                       ['x','B','C','D','E'],
                                       ['x','y','B','C','z','E'],
                                       ['A','B','C','x','y','z','D']])),
                         simplify(msa(['ABCDE',
                                       'xBCDE',
                                       'xyBCzE',
                                       'ABCxyzD'])))

    def test_disjunct(self):
        from msa import Disjunct as d
        self.assertEqual([x for x in d(1,2,3)],
                         [1, 2, 3])
        self.assertEqual(d([1,2,3]),
                         d([1,2,3]))

    def test_pair(self):
        from msa import Disjunct as d, Sub as s
        self.assertEqual(msa.msa(['ABC', 'BCDE']),
                         [d((s('A'),), ()),
                           s('B'), s('C'),
                           d((), (s('D'), s('E')))])
    def test_triple_compact(self):
        from msa import Disjunct as d, Sub as s
        self.assertEqual(msa.compact(msa.msa(['ABC', 'BCD', 'BCX'])),
                         [d((d((s('A'),), ()),), ()),
                          s('BC'),
                          d((d((), (s('D'),)),), (s('X'),))])
    def test_edges(self):
        from msa import to_edges, Edge, Disjunct as d, Sub as s
        def new_edge(f, t):
            return Edge(f.seq, t.seq, f.id(), t.id())
        self.assertEqual(set([x for x in to_edges([d(s('A'), (s('B'), d(s('C'), s('D')))),s('E')])]),
                         set([
                    new_edge(s('START'), s('A')),
                    new_edge(s('START'), s('B')),
                    new_edge(s('B'), s('C')),
                    new_edge(s('B'), s('D')),
                    new_edge(s('E'), s('END')),
                    new_edge(s('C'), s('E')),
                    new_edge(s('D'), s('E')),
                    new_edge(s('A'), s('E')),
                    ]))

if __name__ == '__main__':
    unittest.main()
