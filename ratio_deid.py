#! /usr/bin/env python
# -*- coding:utf-8 -*-

""" heuristically convert the k-anonymized result into the deid result in the token-by-line form. """

import re
import sys

def load(st):
    for line in st:
        a = line.split("\t", 2)
        a = a[0].strip()
        yield a

def convert(system, tag='PHI', ratio=0.5, suppchar='_'):
    from validate_frequency import number_of_matches
    for line in system:
        line = line.strip()
        n = number_of_matches(suppchar, line)
        yield (line, tag if float(n) / len(line) >= ratio else '')

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-w', '--wildcard',
                        dest='wildcard', type=str, default='_',
                        help='')
    parser.add_argument('-r', '--ratio',
                        dest='ratio', type=float, default=0.5,
                        help='')
    parser.add_argument('-t', '--tag',
                        dest='tag', type=str, default='PHI',
                        help='')
    parser.add_argument('-o', '--output', metavar='FILE',
                        dest='output', type=lambda x: open(x, 'w'), default=sys.stdout,
                        help='')
    parser.add_argument('-v', '--verbose',
                        dest='verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('input')
    options = parser.parse_args()

    for (line,label) in convert(load(open(options.input)), options.tag, options.ratio, options.wildcard):
        print '%s\t%s' % (line,label)
