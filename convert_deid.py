#! /usr/bin/env python
# -*- coding:utf-8 -*-

""" Parse the input XML in the format of the groundtruth of i2b2 deidentification challenge, and output token-by-line with the BIO encoding of PHI labels. """

from xml.sax import make_parser, handler
import re
import sys
import random
from collections import defaultdict

class DeidHandler(handler.ContentHandler):

    def __init__(self, pattern, tag, rpattern='RECORD', rattr='ID'):
        self._text = ''
        self._phi = False
        self.pattern = pattern
        self.tag = tag
        self.cur = None
        self.rpattern = rpattern
        self.rattr = rattr
        self.acc = defaultdict(list)

    def startElement(self, name, attrs):
        if name == self.rpattern:
            self.cur = attrs[self.rattr]
        if self.pattern.match(name):
            self._phi = True

    def endElement(self, name):
        if self.pattern.match(name):
            self._phi = False
            
    def characters(self, content):
        tokens = [x for x in re.split(r'\s+', content) if len(x) > 0]
        if len(self.tag) > 0 and self._phi:
            self.acc[self.cur].append("".join(["%s\t%s\n" % (x, self.tag) for x in tokens]))
        else:
            self.acc[self.cur].append("".join([x + "\n" for x in tokens]))

    def get(self):
        return self.acc

def convert(inp, out, pattern=re.compile(r'^PHI'), tag='PHI', shuffle=False):
    parser = make_parser()
    handler = DeidHandler(pattern, tag)
    parser.setContentHandler(handler)
    parser.parse(inp)
    records = handler.get().items()
    if shuffle:
        random.shuffle(records)

    for (key, lines) in records:
        for line in lines:
            out.write(line)

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--pattern',
                        dest='pattern', type=lambda x: re.compile(x), default='^PHI',
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
    parser.add_argument('-s', '--shuffle',
                        dest='shuffle', action='store_true', default=False,
                        help='shuffle records')
    parser.add_argument('input')
    options = parser.parse_args()

    convert(open(options.input), options.output, options.pattern, options.tag)
