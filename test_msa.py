#! /usr/bin/env python

import unittest
import msa

class TestMSA(unittest.TestCase):
    def setUp(self):
        None

    def test_pair(self):
        self.assertEqual(msa.msa(['ABC', 'BCD']),
                         [[(('A',), ()),
                          'B',
                          'C',
                          ((), ('D',))]])
    def test_triple(self):
        self.assertEqual(msa.flatten(msa.msa(['ABC', 'BCD', 'BCX'])),
                         [[[['A'], []], []],
                          'BC',
                          [[[], ['D']], ['X']]])

if __name__ == '__main__':
    unittest.main()
