#! /usr/bin/env python
# -*- coding:utf-8 -*-

import unittest
import wikirevs
from datetime import datetime

def format_date(x):
    return datetime.strftime(x,      '%Y-%m-%dT%H:%M:%SZ')

def parse_date(x):
    return datetime.strptime(str(x), '%Y-%m-%dT%H:%M:%SZ')

def replace_or_write_to_local_mediawiki(title, content, baseurl):
    import mechanize
    br = mechanize.Browser()
    br.open('%s/index.php?title=%s&action=edit' % (baseurl, title))
    br.select_form(name='editform')
    if len(br.form['wpTextbox1']) < len(content):
        br.form['wpTextbox1'] = content
    else:
        br.form['wpTextbox1'] = br.form['wpTextbox1'][0:-len(content)] + content
    br.submit()
    return br.geturl()

class TestWikiRevs(unittest.TestCase):
    def setUp(self):
        None

    def test_get_revisions(self):
        base = 'http://localhost:4881'
        content = ['*', '+']
        title = 'hello'
        url = None
        start = datetime.utcnow()
        for c in content:
            url = replace_or_write_to_local_mediawiki(title, c, base)
        import urllib2
        title = urllib2.unquote((url.split(base + '/index.php/'))[1])
        revs = wikirevs.get_revisions(title, base=base)
        start = format_date(start)
        revs = [x for x in revs if x['timestamp'] > start]
        self.assertEqual(2, len(revs))
        self.assertEqual(['*', '+'], [x['text'][-1] for x in revs])

if __name__ == '__main__':
    unittest.main()
