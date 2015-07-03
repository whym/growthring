#! /usr/bin/env python

""" Extract the distribution of frequencies and sizes of continuous substrings """

import sys
import argparse
import re
import codecs

def stats(inputs, marker):
    from collections import defaultdict
    splitter = re.compile(marker + '+')
    binss = []
    for inp in inputs:
        bins = defaultdict(int)
        segments = re.split(splitter, inp.read())
        for s in segments:
            bins[len(s)] += 1
        binss.append(bins)

    maxv = max([max(x.keys()) for x in binss])
    for bins in binss:
        for i in range(0,maxv):
            bins[i] += 0

    for i in xrange(0,maxv+1):
        yield (i, [binss[j][i] for j in xrange(0,len(binss))])

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('-m', '--marker', type=str, default='_')
    parser.add_argument('-e', '--encoding', type=str, default='utf-8')
    parser.add_argument('-v', '--verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('inputs', nargs='+')
    options = parser.parse_args()
    if options.verbose:
        print >>sys.stderr, options

    if options.inputs == ['-']:
        options.inputs = [codecs.getreader(options.encoding)(sys.stdin)]
    else:
        options.inputs = [codecs.open(x, encoding=options.encoding) for x in options.inputs]
    sys.stderr = codecs.getwriter(options.encoding)(sys.stderr)

    for (i, vals) in stats(options.inputs, options.marker):
        print '%d\t%s' % (i, "\t".join([str(x) for x in vals]))
