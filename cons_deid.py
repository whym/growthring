#! /usr/bin/env python
# -*- coding:utf-8 -*-

""" merge two deid-form text file. """

import re
import sys
from collections import defaultdict

def load(st):
    for line in st:
        a = line.split("\t", 2)
        a = a[0].strip()
        yield a

def merge_trusting_system1_unhidden(system1, system2, wildcard, ratio=0.8):
    from validate_frequency import number_of_matches

    for (s1,s2) in zip(system1,system2):
        n = number_of_matches(wildcard, s1)
        if float(n) / len(s1) >= ratio:
            yield s1
        else:
            yield s2

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-w', '--wildcard',
                        dest='wildcard', type=str, default='_',
                        help='')
    parser.add_argument('-r', '--ratio',
                        dest='ratio', type=float, default=0.8,
                        help='')
    parser.add_argument('-o', '--output', metavar='FILE',
                        dest='output', type=lambda x: open(x, 'w'), default=sys.stdout,
                        help='')
    parser.add_argument('-v', '--verbose',
                        dest='verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('system1')
    parser.add_argument('system2')
    options = parser.parse_args()
    
    for token in merge_trusting_system1_unhidden(load(open(options.system1)), load(open(options.system2)), options.wildcard, options.ratio):
        print >>options.output, token
