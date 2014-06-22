'''This is a proof-of-concept to show it is possible to "concatenate" two suffix arrays that represent two concecutive regions in a string delimited by a special boundary character.
'''
from itertools import *

DELIMIT = '#'

def suffixes(s):
    '''sorted suffixes'''
    return sorted(xrange(0, len(s)), key=lambda i: s[i:])

def merge(left, right, key):
    '''merge sort module

    This *has to be* linear in time, but it is not proven to be so (and I'm not positive to prove so) when "key" is looking at subsequent characters.  What is an efficient alternative?

    '''
    if not len(left) or not len(right):
        return left or right

    result = []
    i, j = 0, 0
    while (len(result) < len(left) + len(right)):
        if key(left[i]) < key(right[j]):
            result.append(left[i])
            i+= 1
        else:
            result.append(right[j])
            j+= 1
        if i == len(left) or j == len(right):
            result.extend(left[i:] or right[j:])
            break 

    return result

def suffixes_delimited(s):
    '''suffixes sorted by the delimiting method'''
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
    for (i,x) in enumerate(a):
        ret = merge(ret, [lens[i] + j for j in suffixes(x)], lambda i: s[i:])
    return ret

if __name__ == '__main__':
    while True:
        s = raw_input('> ').strip() + DELIMIT
        if suffixes_delimited(s) == suffixes(s):
            for i in suffixes_delimited(s):
                print '%10d %s' % (i, s[i:])
        else:
            print 'TEST FAILED'
            for i in suffixes_delimited(s):
                print '%10d %s' % (i, s[i:])
            print '-----'
            for i in suffixes(s):
                print '%10d %s' % (i, s[i:])

