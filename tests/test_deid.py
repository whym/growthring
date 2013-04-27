#! /usr/bin/env python
# -*- coding:utf-8 -*-

import unittest
import convert_deid, evaluate_deid, ratio_deid
from StringIO import StringIO

class TesttDeid(unittest.TestCase):
    def setUp(self):
        None

    def test_evaluate(self):
        system = StringIO('''A__D	PHI
ABCD
GF
''')
        expect = StringIO('''ABCD	PHI
ABCD
GF	PHI
''')
        res = list(evaluate_deid.evaluate([('ABCD', 'PHI'),
                                           ('ABCD', ''),
                                           ('G', 'PHI')],
                                          [('A__D', 'PHI'),
                                           ('ABCD', ''),
                                           ('G', '')]))
        self.assertEqual([('A__D', 'PHI', 'ABCD', 'PHI','TP'),
                          ('ABCD', '', 'ABCD', '', 'TN'),
                          ('G', '', 'G', 'PHI','FP')], res)

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
        from xml.sax import make_parser
        parser = make_parser()
        out = StringIO()
        inp = StringIO('<ROOT><REPORT><TEXT>a <PHI>b</PHI> c</TEXT></REPORT></ROOT>')
        convert_deid.convert(inp, out)
        self.assertEqual('''a
b	PHI
c
''', out.getvalue())

if __name__ == '__main__':
    unittest.main()
