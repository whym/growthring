#! /usr/bin/env python
# -*- coding:utf-8 -*-

""" Split input text with the hiding marker ('_' by default), count frequencies of all strings and substrings, and ensure they all above the given the threshold.  By default instead of attempting all substrings, it randomly chooses and tests 1000 substrings. """

if __name__ == '__main__':

    import sys
    import argparse
    import re
    import codecs
    import random

    parser = argparse.ArgumentParser()
    parser.add_argument('-t', '--threshold', type=int, default=2)
    parser.add_argument('-n', '--tests', type=int, default=1000)
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

    splitter = re.compile(options.marker + '+')
    for f in options.inputs:
        lines = f.read()
        segments = re.split(splitter, lines)

        random.shuffle(segments)
        for s in segments[0:options.tests]:
            n = len([x for x in re.finditer(re.escape(s), lines)])
            print n, repr(s)
        