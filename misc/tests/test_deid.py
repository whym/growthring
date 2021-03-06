#! /usr/bin/env python
# -*- coding:utf-8 -*-

import unittest
import convert_deid, evaluate_deid, freq_deid
from StringIO import StringIO

class TestDeid(unittest.TestCase):
    def setUp(self):
        None

    def test_fmeasure(self):
        flags = {(True,  True):  2,
                 (True,  False): 8,
                 (False, False): 4,
                 (False, True):  2,
        }
        p = 0.5
        r = 0.2
        self.assertEqual((2/(1/p + 1/r), p, r),
                         evaluate_deid.fmeasure_precition_recall(flags))
        flags = {(True,  True):  0,
                 (True,  False): 5,
                 (False, False): 0,
                 (False, True):  5,
        }
        self.assertEqual((0, 0, 0),
                         evaluate_deid.fmeasure_precition_recall(flags))

    def test_count_ngrams(self):
        res = freq_deid.count_ngrams(['A', 'B', 'C', 'A', 'B'], 2)
        self.assertEqual({('A', 'B'): 2, ('B', 'C'): 1, ('C', 'A'): 1}, res)

    def test_evaluate(self):
        res = list(evaluate_deid.evaluate([('ABCD', 'PHI'),
                                           ('ABCD', ''),
                                           ('G', 'PHI'),
                                           ('X', '')],
                                          [('A__D', 'PHI'),
                                           ('ABCD', ''),
                                           ('G', ''),
                                           ('X', 'PHI')]))
        self.assertEqual([('A__D', 'PHI', 'ABCD', 'PHI','TP'),
                          ('ABCD', '', 'ABCD', '', 'TN'),
                          ('G', '', 'G', 'PHI','FN'),
                          ('X', 'PHI', 'X', '','FP')], res)

    def test_ratio(self):
        inp = StringIO('''A__D	
ABCD
_
''')
        res = list(evaluate_deid.convert(inp, ratio=0.5))
        self.assertEqual([('A__D', 'PHI'),
                          ('ABCD', ''),
                          ('_', 'PHI')],
                         res)

    def test_convert(self):
        out = StringIO()
        inp = StringIO('<ROOT><RECORD ID="123"><TEXT>a <PHI>b</PHI> c</TEXT></RECORD></ROOT>')
        convert_deid.convert(inp, out)
        self.assertEqual('''a
b	PHI
c
''', out.getvalue())

if __name__ == '__main__':
    unittest.main()
