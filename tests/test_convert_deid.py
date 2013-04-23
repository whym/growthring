#! /usr/bin/env python
# -*- coding:utf-8 -*-

import unittest
import convert_deid
from datetime import datetime

class TestConvertDeid(unittest.TestCase):
    def setUp(self):
        None

    def test0(self):
        from xml.sax import make_parser
        parser = make_parser()
        from StringIO import StringIO
        out = StringIO()
        inp = StringIO()
        inp = StringIO('<ROOT><REPORT><TEXT>a <PHI>b</PHI> c</TEXT></REPORT></ROOT>')
        convert_deid.convert(inp, out)
        self.assertEqual('''a
b	PHI
c
''', out.getvalue())

if __name__ == '__main__':
    unittest.main()
