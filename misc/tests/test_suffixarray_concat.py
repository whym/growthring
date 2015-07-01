#! /usr/bin/env python
# -*- coding:utf-8 -*-

import unittest
import suffixarray_concat as sc
from StringIO import StringIO

class TestSuffixArrayConcat(unittest.TestCase):
    def setUp(self):
        None

    def test_concat(self):
        s = sc.DELIMIT.join(['mississippi', 'Mississippi'])
        self.assertEqual(sc.suffixes_delimit_merge(s), [len(s)]+sc.suffixes(s))

if __name__ == '__main__':
    unittest.main()
