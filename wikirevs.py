#! /usr/bin/env python
# -*- coding:utf-8 -*-

"""DESCRIBE THIS PROGRAM"""

def retrieve(url, sleep=5, trials=20):
    import urllib2
    import sys
    import time
    headers = {'User-Agent':
                   'Mozilla/5.0 (Windows; U; Windows NT 5.1; it; rv:1.8.1.11) Gecko/20071127 Firefox/2.0.0.11'}
    i = 0
    while i < trials:
        i += 1
        try:
            print >>sys.stderr, 'fetching %s' % url
            res = urllib2.build_opener(urllib2.HTTPRedirectHandler()).open(\
                urllib2.Request(url,
                                headers=headers))
            break
        except urllib2.URLError:
            time.sleep(sleep)
    else:
        return None
    content = res.read()
    return content

def get_revisions(page, base, sleep=5):
    # retrieve the content in the XML format
    import urllib2
    url = '%s/index.php?title=Special:Export&pages=%s&history' % (base, urllib2.quote(page))
    xml = retrieve(url) 
    from xml.dom import minidom
    page = minidom.parseString(xml).getElementsByTagName('page')[0]
    attrs = ['id', 'timestamp', 'sha1', 'text']
    ret = []
    for rev in page.getElementsByTagName('revision'):
        ent = {}
        for a in attrs:
            v = rev.getElementsByTagName(a)[0].childNodes[0].data
            ent[a] = v
        ret.append(ent)
    return ret
            
if __name__ == '__main__':

    import sys
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--verbose', action='store_true', default=False,
                        help='turn on verbose message output')
    parser.add_argument('-o', '--output', type=str, default='-',
                        help='')
    parser.add_argument('-b', '--baseurl', default='http://en.wikipedia.org/w/')
    parser.add_argument('-f', '--format', choices=['json', 'plain'], default='plain',
                        help='')
    parser.add_argument('title')
    options = parser.parse_args()
    if options.verbose:
        print >>sys.stderr, options

    if options.output == '-':
        options.output = sys.stdout
    else:
        options.output = open(options.output, 'w')

    revs = get_revisions(options.title, options.baseurl)
    if options.format == 'json':
        import json
        json.dump(revs, options.output)
    else:
        for r in revs:
            print >>options.output, repr(r['text'])
