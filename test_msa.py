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

    def test_disjunct(self):
        from msa import Disjunct as d
        self.assertEqual([x for x in d(1,2,3)],
                         [1, 2, 3])
        self.assertEqual(d([1,2,3]),
                         d([1,2,3]))

    def test_pair(self):
        from msa import Disjunct as d
        self.assertEqual(msa.msa(['ABC', 'BCDE']),
                         [d(('A',), ()),
                           'B', 
                           'C',
                           d((), ('D', 'E'))])
    def test_triple_compact(self):
        from msa import Disjunct as d
        self.assertEqual(msa.compact(msa.msa(['ABC', 'BCD', 'BCX'])),
                         [d((d(('A',), ()),), ()),
                          'BC',
                          d((d((), ('D',)),), ('X',))])
    def test_edges(self):
        from msa import to_edges, Edge, Disjunct as d
        self.assertEqual(set([x for x in to_edges([d('A', ('B', d('C', 'D'))),'E'])]),
                         set([
                    Edge(from_='START', to='A'),
                    Edge(from_='START', to='B'),
                    Edge(from_='B', to='C'),
                    Edge(from_='B', to='D'),
                    Edge(from_='E', to='END'),
                    Edge(from_='C', to='E'),
                    Edge(from_='D', to='E'),
                    Edge(from_='A', to='E'),
                    ]))

if __name__ == '__main__':
    unittest.main()
