'''This is a proof-of-concept script to show it is possible to "concatenate" two suffix arrays that represent two concecutive regions in a string. It assumes the regions are delimited by a special boundary character.
'''
import argparse

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

def suffixes_delimit(s):
    # split
    a = [x + DELIMIT for x in s.split(DELIMIT)]
    if a[-1] == DELIMIT:
        a.pop()

    # prepare offsets
    lens = [0]
    for x in a:
        lens.append(lens[-1] + len(x))
    lens.pop()
    for (i,x) in enumerate(a):
        yield [lens[i] + j for j in suffixes(x)]

def suffixes_delimit_merge(s):
    '''suffixes sorted by the delimit-and-merge method'''
    # merge suffix arrays for splits
    ret = []
    for (i,ls) in enumerate(suffixes_delimit(s)):
        ret = merge(ret, ls, lambda i: s[i:])
    return ret

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
        if suffixes_delimit_merge(s) == suffixes(s):
            if options.verbose:
                for (n,ls) in enumerate(suffixes_delimit(s)):
                    for i in ls:
                        print '%3d %10d %s' % (n, i, s[i:])
                
            for i in suffixes_delimit_merge(s):
                print '%10d %s' % (i, s[i:])
        else:
            print 'TEST FAILED'
            for i in suffixes_delimit_merge(s):
                print '%10d %s' % (i, s[i:])
            print '-----'
            for i in suffixes(s):
                print '%10d %s' % (i, s[i:])

