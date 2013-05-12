#! /usr/bin/env python
# -*- coding: utf-8 -*-

if __name__ == '__main__':
    import sys
    import argparse
    from collections import defaultdict
    parser = argparse.ArgumentParser()
    parser.add_argument('-o', '--output', metavar='FILE',
                        dest='output', type=lambda x: codecs.open(x, 'w', encoding='utf_8'), default=sys.stdout,
                        help='')
    parser.add_argument('-s', '--skip-identical', default=False, action='store_true')
    parser.add_argument('inputs', nargs='+')
    options = parser.parse_args()

    if options.rank:
        valuef = rank
    else:
        valuef = rank_binary(options.nbest)

    agg = defaultdict(list)
    for f in options.inputs:
        d = json.load(open(f))
        for entry in d['indiv']:
            v = valuef(entry)
            agg[entry['id']].append(v)
    
    contig = defaultdict(int)
    writer = csv.writer(options.output)
    writer.writerow(['# id'] + options.inputs)
    for (id, vals) in sorted(agg.items(), key=lambda x: x[0]):
        if not options.skip_identical or not all([x == vals[0] for x in vals]):
            writer.writerow([id] + vals)
        contig[tuple(vals)] += 1
    writer.writerow(['# %s' % repr(contig)])

    if len(options.inputs) == 2:
        s = float(sum(contig.values()))
        writer.writerow(['# 1st method: %f' % (((contig[(True,True)] + contig[(True,False)]) / s )) ])
        writer.writerow(['# 2nd method: %f' % (((contig[(True,True)] + contig[(False,True)]) / s )) ])
        import mcnemar
        s = 'no' if mcnemar.mcnemar(contig[(True,True)],
                                    contig[(True,False)],
                                    contig[(False,True)],
                                    contig[(False,False)],
                                    alpha = 0.05, verbose=False) else 'yes'
        writer.writerow(['# mcnemar significance: %s' % s])
        
