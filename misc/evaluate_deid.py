#! /usr/bin/env python
# -*- coding:utf-8 -*-

""" compare two deid-form text file. """

import sys
from collections import defaultdict

def load_unlabeled(st):
    for line in st:
        a = line.split("\t", 2)
        a = a[0].strip()
        yield a

def load_labeled(st):
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
        yield (stoken,slabel,etoken,elabel, ('T' if slabel == elabel else 'F') + ('N' if slabel == '' else 'P'))

def mcnemar_significance(contig):
    import mcnemar
    significant = not mcnemar.mcnemar(contig[(True,True)],
                                      contig[(True,False)],
                                      contig[(False,True)],
                                      contig[(False,False)],
                                      alpha = 0.05, verbose=False)
    return significant

def fmeasure_precition_recall(flags):
    p = float(flags[(True,True)]) / (flags[(False,True)] + flags[(True,True)])
    r = float(flags[(True,True)]) / (flags[(True,False)] + flags[(True,True)])
    f = 2.0 / ( ( 1.0 / p ) + ( 1.0 / r ) ) if p > 0 and r > 0 else 0
    return (f, p, r)

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
                        dest='ratio', type=lambda x: [float(y) for y in x.split(',')], default=[0.5],
                        help='')
    parser.add_argument('-t', '--tag',
                        dest='tag', type=str, default='PHI',
                        help='')
    parser.add_argument('-o', '--output', metavar='FILE',
                        dest='output', type=lambda x: open(x, 'w'), default=sys.stdout,
                        help='')
    parser.add_argument('-d', '--detail-output', metavar='FILE',
                        dest='doutput', type=lambda x: open(x, 'w'), default=sys.stdout,
                        help='')
    parser.add_argument('-s', '--skip-significance',
                        dest='skipsig', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('-v', '--verbose',
                        dest='verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('expect')
    parser.add_argument('system', nargs='+')
    options = parser.parse_args()
    
    import csv
    writer = csv.writer(options.output)
    writer.writerow(['# filename', 'threshold', 'total', 'tp', 'tn', 'fp', 'fn', 'sig', 'fmeas', 'prec', 'reca'])
    for system in options.system:
        for ratio in options.ratio:
            contig = defaultdict(int)
            for (stoken,slabel,etoken,elabel,res) in evaluate(load_labeled(open(options.expect)),
                                                              convert(load_unlabeled(open(system)), options.tag, ratio, options.wildcard),
                                                              options.wildcard):
                print >>options.doutput, '# %s\t%s\t%s\t%s\t%s' % (stoken,slabel,etoken,elabel,res)
                contig[(elabel != '', slabel != '')] += 1

            sig = 'unknown'
            fm = ['nan','nan','nan']
            if len(contig.values()) > 0 and min(contig.values()) > 0:
                if not options.skipsig:
                    sig = 'yes' if mcnemar_significance(contig) else 'no'
                fm = ['%.4f' % x for x in fmeasure_precition_recall(contig)]
            writer.writerow([system,
                             ratio,
                             sum(contig.values()),
                             contig[(True,True)],
                             contig[(False,False)],
                             contig[(False, True)],
                             contig[(True,False)],
                             sig] + fm)

