#! /usr/bin/env python
# -*- coding:utf-8 -*-

import unittest
import stats
from StringIO import StringIO

class TestStats(unittest.TestCase):
    def setUp(self):
        None

    def test_stats(self):
        inp = StringIO('''A__DE_G_IJK__''')
        res = [x for x in stats.stats([inp], '_')]
        self.assertEqual([(0, [1]),
                          (1, [2]),
                          (2, [1]),
                          (3, [1])], res)

if __name__ == '__main__':
    unittest.main()
