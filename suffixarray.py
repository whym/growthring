from itertools import *

DELIMIT = '$'

def suffixes(s):
    return sorted(xrange(0, len(s)), key=lambda i: s[i])

def suffixes_delimited(s):
    # split
    a = [x + DELIMIT for x in s.split(DELIMIT)]
    if a[-1] == DELIMIT:
        a.pop()

    # prepare offsets
    lens = [0]
    for x in a:
        lens.append(lens[-1] + len(x))
    lens.pop()

    # merge suffix arrays for splits
    ret = []
    for x in [[lens[i] + j] for j in suffixes(x) for (i,x) in enumerate(a)]:
        ret += x
    return ret

if __name__ == '__main__':
    while True:
        s = raw_input('> ').strip() + DELIMIT
        for i in suffixes_delimited(s):
            print '%10d %s' % (i, s[i:])
        print '-----'
        for i in suffixes(s):
            print '%10d %s' % (i, s[i:])

