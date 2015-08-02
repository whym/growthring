#! /usr/bin/env python
# -*- coding:utf-8 -*-

"""Output comparison of N-gram frequencies of sensitive terms and non-sensitive spans, where N is a given intege. Input is a two-column labeled deid format. """

import sys
from collections import defaultdict
import matplotlib.pyplot as plt
import csv
import math


def load(st):
    for line in st:
        a = line.split("\t", 2)
        a[0] = a[0].strip()
        if len(a) == 0:
            a.append('EOS')
        if len(a) == 1:
            a.append('')
        a[1] = a[1].strip()
        yield tuple(a)


def count_ngrams(tokens, size):
    ng = defaultdict(int)
    buff = tokens[0:size]
    ng[tuple(buff)] = 1
    for t in tokens[size:]:
        buff = buff[1:]
        buff.append(t)
        ng[tuple(buff)] += 1
    return ng


def binned(vals, binsize, mx=None, logarithmic=False):
    if logarithmic:
        scale = lambda x: int(math.log10(x) / binsize)
        unscale = lambda x: 10 ** (x * binsize)
    else:
        scale = lambda x: int(x / binsize)
        unscale = lambda x: x * binsize
    if mx is None:
        mx = max(vals)
    c = defaultdict(int)
    for i in xrange(0, scale(mx)):
        c[i] = 0
    for n in vals:
        if n > mx:
            continue
        c[scale(n)] += 1
    for (k, v) in sorted(c.items()):
        yield unscale(k), unscale(k + 1), v


def csv_histogram(serieses, output, binsize=10, logarithmic=False):
    bn = {}
    for (s, ls) in serieses.items():
        bn[s] = list(binned(ls, binsize, 5000, logarithmic))
    names = serieses.keys()
    writer = csv.writer(output)
    writer.writerow(['b_start', 'b_end'] + names)
    for i in xrange(0, len(bn[names[0]])):
        writer.writerow(list(bn[names[0]][i][0:2]) +
                        [bn[s][i][2] for s in names])


def plot_histogram(serieses, output, binsize=10, logarithmic=False):
    plt.bar(left=[1, 2, 3], height=[10, 30, 20], width=0.2, color='red')
    plt.bar(left=[1.2, 2.2, 3.2], height=[5, 10, 20], width=0.2, color='blue')
    plt.show()

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-o', '--output', metavar='FILE',
                        dest='output', type=lambda x: open(x, 'w'), default=sys.stdout,
                        help='')
    parser.add_argument('-n', '--ngram', metavar='N',
                        dest='ngram', type=int, default=1,
                        help='')
    parser.add_argument('-b', '--binsize', metavar='N',
                        dest='bin', type=float, default=10,
                        help='')
    parser.add_argument('-l', '--logarithmic',
                        dest='logarithmic', action='store_true', default=False,
                        help='')
    parser.add_argument('-v', '--verbose',
                        dest='verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('input')
    options = parser.parse_args()

    tokens = list(load(open(options.input)))
    ngrams = count_ngrams([x[0] for x in tokens], options.ngram)
    serieses = defaultdict(list)

    for i in xrange(0, len(tokens) - options.ngram):
        f = ngrams[tuple(x[0] for x in tokens[i:i + options.ngram])]
        serieses[tokens[i][1]].append(f)
        if options.verbose:
            print tokens[i][0], tokens[i][1], f
    csv_histogram(serieses, options.output, binsize=options.bin,
                  logarithmic=options.logarithmic)
