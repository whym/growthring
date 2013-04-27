#! /usr/bin/env python
# -*- coding:utf-8 -*-

""" compare two deid-form text file. """

import re
import sys
from collections import defaultdict

def load(st):
    for line in st:
        a = [x.strip() for x in line.split("\t", 2)]
        if len(a) == 0:
            a.append('')
            a.append('')
        if len(a) == 1:
            a.append('')
        yield a

def match(stoken, etoken, supp):
    if len(stoken) != len(etoken):
        return False
    for (s,e) in zip(stoken,etoken):
        if s == supp or e == supp or s == e:
            next
        else:
            return False
    return True

def evaluate(expect, system, suppchar='_'):
    for ((stoken,slabel),(etoken,elabel)) in zip(system, expect):
        if not match(stoken,etoken,suppchar):
            print >>sys.stderr, 'error: %s vs %s' % (etoken,stoken)
            next
        yield (stoken,slabel,etoken,elabel, ('T' if slabel == elabel else 'F') + ('N' if elabel == '' else 'P'))

def mcnemar_significance(contig):
    import mcnemar
    significant = not mcnemar.mcnemar(contig[(True,True)],
                                      contig[(True,False)],
                                      contig[(False,True)],
                                      contig[(False,False)],
                                      alpha = 0.05, verbose=False)
    return significant

def fmeasure_precition_recall(flags):
    print flags
    p = float(flags[(True,True)]) / (flags[(False,True)] + flags[(True,True)])
    r = float(flags[(True,True)]) / (flags[(True,True)] + flags[(True,False)])
    return (2.0 / ( ( 1.0 / p ) + ( 1.0 / r ) ), p, r)

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-w', '--wildcard',
                        dest='wildcard', type=str, default='_',
                        help='')
    parser.add_argument('-o', '--output', metavar='FILE',
                        dest='output', type=lambda x: open(x, 'w'), default=sys.stdout,
                        help='')
    parser.add_argument('-v', '--verbose',
                        dest='verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('expect')
    parser.add_argument('system')
    options = parser.parse_args()
    
    contig = defaultdict(int)
    for (stoken,slabel,etoken,elabel,res) in evaluate(load(open(options.expect)), load(open(options.system)), options.wildcard):
        print >>options.output, '%s\t%s\t%s\t%s\t%s' % (stoken,slabel,etoken,elabel,res)
        contig[(elabel != '', slabel != '')] += 1

    print >>sys.stderr, 'mcnemar significance: %s' % ('yes' if mcnemar_significance(contig) else 'no')
    print >>sys.stderr, 'f measure: %.3f (p %.3f, r %.3f)' % fmeasure_precition_recall(contig)
