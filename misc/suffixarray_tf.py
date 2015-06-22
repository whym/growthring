'''

@inproceedings{narisawa2007efficient,
  title={Efficient computation of substring equivalence classes with suffix arrays},
  author={Narisawa, Kazuyuki and Inenaga, Shunsuke and Bannai, Hideo and Takeda, Masayuki},
  booktitle={Combinatorial Pattern Matching},
  pages={340--351},
  year={2007},
  organization={Springer}
}

'''
from itertools import *
from IPython import embed
import argparse

DELIMIT = '#'

def suffixes(s):
    '''sorted suffixes, computed naively'''
    return sorted(xrange(0, len(s)), key=lambda i: s[i:])

def all_substrings(s):
    length = len(s)
    return (s[i: j] for i in xrange(length) for j in xrange(i + 1, length + 1))

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
    stack = [(-1, -1)]
    for i in xrange(0, len(sa)):
        l_new = i - 1
        h_new = lcp[i]
        left = stack[-1][0]
        height = stack[-1][1]
        while height > h_new:
            stack.pop()
            if stack[-1][1] > h_new:
                parent = stack[-1][1]
            else:
                parent = h_new
            l = left
            r = i - 1
            freq = r - l + 1
            rlen = height
            bl = l
            br = r
            if sa[l] != 1 and sa[r] != 1:
                bl = rank[sa[l] - 1]
                br = rank[sa[r] - 1]
            if (br - bl + 1 != freq) or (w[bl] != w[br]) or (sa[l] == 1) or (sa[r] == 1):
                x = w[sa[l]: sa[l] + rlen]
                size = rlen - parent
                mlen = rlen - parent
                ln = rlen
                minimal = set()
                fl = rank[sa[l] + 1]
                fr = rank[sa[r] + 1]
                bl = l
                br = r
                while (ln - 1 > lcp[fl]) and (ln-1 > lcp[fr+1]) and (fr - fl +1 == freq):
                    if lcp[fl] >= lcp[fr+1]:
                        parent = lcp[fl]
                    else:
                        parent = lcp[fr + 1]
                    ln = ln = 1
                    size = size + ln - parent
                    if mlen > ln - parent:
                        minimal.add((sa[bl], sa[bl] + parent))
                    bl = fl
                    br = fr
                    if (sa[fl] + 1 >= len(w)) or (sa[fr] + 1 >= len(w)):
                        break
                    fl = rank[sa[fl] + 1]
                    fr = rank[sa[fr] + 1]
                    mlen = ln - parent
                minimal.add((sa[bl], sa[bl] + parent))
                print '"%s": freq %d, size %d, %s' % (x, freq, size, minimal)
                #print pair2str(w, (sa[l], sa[l] + rlen - 1)), '--', ', '.join([pair2str(w, y) for y in minimal])
                print ' --', [x for x in all_substrings(x) if any(x.find(w[y[0]:y[1]+1]) >= 0 for y in minimal)]
            l_new = left
            left = stack[-1][0]
            height = stack[-1][1]
        if height < h_new:
            stack.append((l_new, h_new))
        stack.append((i, len(w) - sa[i]))

def next_input(args):
    for x in args:
        yield x
    while True:
        yield raw_input('> ').strip()

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('inputs', nargs='*')
    options = parser.parse_args()
    for line in next_input(options.inputs):
        s = line + DELIMIT
        sa = suffixes(line)
        rank = [len(s) - 1] * (len(sa) + 2)
        for (x,y) in enumerate(sa):
            rank[y] = x
        lcp = longest_common_prefixes(s, sa) + [-1]

        # index to start from 1
        s = DELIMIT + s
        sa = [x + 1 for x in sa]
        lcp = [-1] + lcp

        print sa, rank, lcp
        ec(s, sa, lcp, rank)
