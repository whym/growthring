#! /usr/bin/env python
# -*- coding:utf-8 -*-

""" Parse the input XML in the format of the groundtruth of i2b2 deidentification challenge, and output token-by-line with the BIO encoding of PHI labels. """

from xml.sax import make_parser, handler
import re
import sys

class DeidHandler(handler.ContentHandler):

    def __init__(self, output, pattern, tag):
        self.out = output
        self._text = ''
        self._phi = False
        self.pattern = pattern
        self.tag = tag

    def startElement(self, name, attrs):
        if self.pattern.match(name):
            self._phi = True

    def endElement(self, name):
        if self.pattern.match(name):
            self._phi = False
            
    def characters(self, content):
        tokens = [x for x in re.split(r'\s+', content) if len(x) > 0]
        if len(self.tag) > 0 and self._phi:
            self.out.write("".join(["%s\t%s\n" % (x, self.tag) for x in tokens]))
        else:
            self.out.write("".join([x + "\n" for x in tokens]))

def convert(inp, out, pattern=re.compile(r'^PHI'), tag='PHI'):
    parser = make_parser()
    parser.setContentHandler(DeidHandler(out, pattern, tag))
    parser.parse(inp)

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
    parser.add_argument('input')
    options = parser.parse_args()

    convert(open(options.input), options.output, options.pattern, options.tag)
