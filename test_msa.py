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

if __name__ == '__main__':
    unittest.main()
