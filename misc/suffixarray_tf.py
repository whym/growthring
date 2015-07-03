'''

@inproceedings{narisawa2007efficient,
  title={Efficient computation of substring equivalence classes with suffix arrays},
  author={Narisawa, Kazuyuki and Inenaga, Shunsuke and Bannai, Hideo and Takeda, Masayuki},
  booktitle={Combinatorial Pattern Matching},
  pages={340--351},
  year={2007},
  organization={Springer}
}

where at line 11, algorithm 2, page 349,

w[BL] != w[BR]

should read as

w[SA[BL]] != w[SA[BR]]

'''
from IPython import embed
import argparse
import codecs
import sys
import math
from collections import namedtuple

EC = namedtuple('EC', ('pos', 'freq', 'minimal'))
ST = namedtuple('ST', ('left', 'height'))
DELIMIT = '#'

def suffixes(s):
    '''sorted suffixes, computed naively'''
    return sorted(xrange(0, len(s)), key=lambda i: s[i:])

def all_subspans(a, b):
    for j in xrange(a, b+1):
        for i in xrange(a, b+1):
            if i < j:
                yield (i, j)

# def pincer(w, pos, m):
#     '''
#     This implementation considers subspan relations, as opposed to substring relations which were ggested in Narisawa (2007).
#      '''
#     for x in all_subspans(pos[0],pos[1]):
#         if any(x[0] <= y[0] and x[1] >= y[1] for y in m):
#             yield w[x[0]:x[1]]

def pincer(w, pos, m):
    for x in all_subspans(pos[0],pos[1]):
        if any(w[x[0]:x[1]].find(w[y[0]:y[1]]) >= 0 for y in m):
            yield w[x[0]:x[1]]

def common_prefix_len(x,y):
    i = 0
    while i < len(x) and i < len(y) and x[i] == y[i]:
        i += 1
    return i

def longest_common_prefixes(s, sa):
    '''LCP computed naively'''
    ret = [-1]
    for i in xrange(1,len(sa)):
        ret.append(common_prefix_len(s[sa[i]:], s[sa[i-1]:]))
    return ret

def pair2str(w, pair):
    x, y = pair
    return 'w[%d:%d] ("%s")' % (x, y, w[x:y+1])

def ec(w, sa, lcp, rank):
    '''
    All arrays in the arguments 1-start.

    EC.pos is exclusive; w[pos[0]:pos[1]] is the substring it represents.
    '''
    lenw = len(w) - 1
    stack = [ST(-1, -1)]
    for i in xrange(1, lenw+1):
        l_new = i - 1
        h_new = lcp[i]
        left = stack[-1].left
        height = stack[-1].height
        while height > h_new:
            stack.pop()
            if stack[-1].height > h_new:
                parent = stack[-1].height
            else:
                parent = h_new
            l = left
            r = i - 1
            freq = r - l + 1
            rlen = height
            bl = 0
            br = 0
            pos = (sa[l], sa[l]+rlen)
            if sa[l] != 1 and sa[r] != 1:
                bl = rank[sa[l] - 1]
                br = rank[sa[r] - 1]
            if (br - bl + 1 != freq) or (w[sa[bl]] != w[sa[br]]) or (sa[l] == 1) or (sa[r] == 1):
                #print w[pos[0]:pos[1]], bl, br, l, r, br - bl + 1 != freq, w[bl] != w[br], sa[l] == 1, sa[r] == 1
                size = rlen - parent
                mlen = rlen - parent
                ln = rlen
                minimal = set()
                fl = rank[sa[l] + 1]
                fr = rank[sa[r] + 1]
                bl = l
                br = r
                while (ln - 1 > lcp[fl]) and (ln - 1 > lcp[fr+1]) and (fr - fl +1 == freq):
                    if lcp[fl] >= lcp[fr+1]:
                        parent = lcp[fl]
                    else:
                        parent = lcp[fr + 1]
                    ln = ln - 1
                    size = size + ln - parent
                    if mlen > ln - parent:
                        minimal.add((sa[bl], sa[bl] + parent + 1))
                    bl = fl
                    br = fr
                    if (sa[fl] + 1 >= lenw) or (sa[fr] + 1 >= lenw):
                        break
                    fl = rank[sa[fl] + 1]
                    fr = rank[sa[fr] + 1]
                    mlen = ln - parent
                minimal.add((sa[bl], sa[bl] + parent + 1))
                #print '"%s" (%d,%d): freq %d, size %d, %s' % (w[pos[0]:pos[1]], pos[0], pos[1], freq, size, minimal)
                #print pair2str(w, (sa[l], sa[l] + rlen - 1)), '--', ', '.join([pair2str(w, y) for y in minimal])
                #print ' --', list(pincer(w, pos, minimal))
                yield EC(pos, freq, [x for x in minimal])
            l_new = left
            left = stack[-1].left
            height = stack[-1].height
        if height < h_new:
            stack.append(ST(l_new, h_new))
        stack.append(ST(i, lenw - sa[i]))

def next_input(args):
    for x in args:
        yield x
    try:
        while True:
            s = raw_input('> ').strip()
            print ''
            yield s
    except EOFError:
        pass

def equivalence_classes(s):
    sa = suffixes(s)
    rank = [-1] * (len(sa) + 2)
    for (x,y) in enumerate(sa):
        rank[y] = x
    lcp = longest_common_prefixes(s, sa) + [0]

    # index to start from 1
    s = DELIMIT + s
    sa = [0] + [x + 1 for x in sa]
    lcp = [-1] + lcp
    rank = [0] + [x + 1 for x in rank]

    #print sa, rank, lcp
    return [EC((x.pos[0]-1, x.pos[1]-1), x.freq, set((y[0]-1,y[1]-1) for y in x.minimal)) for x in ec(s, sa, lcp, rank)]

if __name__ == '__main__':
    sys.stdout = codecs.getwriter('utf-8')(sys.stdout)

    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('-i', '--interactive', action='store_true', default=False,
                        help='turn on interactive mode')
    parser.add_argument('inputs', nargs='*')
    options = parser.parse_args()
    if options.interactive:
        for line in next_input(options.inputs):
            s = line + DELIMIT
            for e in equivalence_classes(s):
                print e, s[e.pos[0]:e.pos[1]]
    else:
        if len(options.inputs) == 0:
            print >>sys.stderr, 'input file missing'
            exit(1)
        lines = []
        for f in options.inputs:
            for line in codecs.open(f, encoding='utf-8'):
                lines.append(line + DELIMIT)
        s = ''.join(lines)
        for e in sorted(equivalence_classes(s), key=lambda e: -(e.pos[1]-e.pos[0])*math.log(e.freq)):
            print e
            print s[e.pos[0]:e.pos[1]].replace('\n', '\\n')
        
