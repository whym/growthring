#! /usr/bin/env python
# -*- coding:utf-8 -*-

import unittest
import wikirevs
from datetime import datetime
from httpretty import HTTPretty, httprettified

def format_date(x):
    return datetime.strftime(x,      '%Y-%m-%dT%H:%M:%SZ')

def parse_date(x):
    return datetime.strptime(str(x), '%Y-%m-%dT%H:%M:%SZ')

class TestWikiRevs(unittest.TestCase):
    def setUp(self):
        None

    @httprettified
    def test_get_revisions(self):
        base = 'http://localhost:4881'
        title = 'hello'

        HTTPretty.register_uri(HTTPretty.GET,
                               base + '/index.php?title=Special:Export&pages=%s&history',
                               body='''<mediawiki xmlns="http://www.mediawiki.org/xml/export-0.10/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.mediawiki.org/xml/export-0.10/ http://www.mediawiki.org/xml/export-0.10.xsd" version="0.10" xml:lang="ja">
  <siteinfo>
    <sitename>MyLocalWiki</sitename>
    <dbname>my_wiki_120</dbname>
    <base>http://localhost:4881/index.php/%E3%83%A1%E3%82%A4%E3%83%B3%E3%83%9A%E3%83%BC%E3%82%B8</base>
    <generator>MediaWiki 1.26alpha</generator>
    <case>case-sensitive</case>
    <namespaces>
      <namespace key="-2" case="case-sensitive">メディア</namespace>
      <namespace key="-1" case="first-letter">特別</namespace>
      <namespace key="0" case="case-sensitive" />
      <namespace key="1" case="case-sensitive">トーク</namespace>
      <namespace key="2" case="first-letter">利用者</namespace>
      <namespace key="3" case="first-letter">利用者・トーク</namespace>
      <namespace key="4" case="case-sensitive">MyLocalWiki</namespace>
      <namespace key="5" case="case-sensitive">MyLocalWiki・トーク</namespace>
      <namespace key="6" case="case-sensitive">ファイル</namespace>
      <namespace key="7" case="case-sensitive">ファイル・トーク</namespace>
      <namespace key="8" case="first-letter">MediaWiki</namespace>
      <namespace key="9" case="first-letter">MediaWiki・トーク</namespace>
      <namespace key="10" case="case-sensitive">テンプレート</namespace>
      <namespace key="11" case="case-sensitive">テンプレート・トーク</namespace>
      <namespace key="12" case="case-sensitive">ヘルプ</namespace>
      <namespace key="13" case="case-sensitive">ヘルプ・トーク</namespace>
      <namespace key="14" case="case-sensitive">カテゴリ</namespace>
      <namespace key="15" case="case-sensitive">カテゴリ・トーク</namespace>
      <namespace key="828" case="case-sensitive">モジュール</namespace>
      <namespace key="829" case="case-sensitive">モジュール・トーク</namespace>
    </namespaces>
  </siteinfo>
  <page>
    <title>hel</title>
    <ns>0</ns>
    <id>680</id>
    <revision>
      <id>1872</id>
      <timestamp>2015-07-02T05:50:17Z</timestamp>
      <contributor>
        <ip>127.0.0.1</ip>
      </contributor>
      <comment>ページの作成:「*」</comment>
      <model>wikitext</model>
      <format>text/x-wiki</format>
      <text xml:space="preserve" bytes="1">*</text>
      <sha1>q37hcqzkhu1rsia7lbtbi2g7lh7y9gh</sha1>
    </revision>
    <revision>
      <id>1873</id>
      <parentid>1872</parentid>
      <timestamp>2015-07-02T05:50:31Z</timestamp>
      <contributor>
        <ip>127.0.0.1</ip>
      </contributor>
      <model>wikitext</model>
      <format>text/x-wiki</format>
      <text xml:space="preserve" bytes="1">+</text>
      <sha1>jsojwe0y00ox745tisb3rfxjb66ayl8</sha1>
    </revision>
  </page>
</mediawiki>
''',
                               content_type="text/xml")
        revs = wikirevs.get_revisions(title, base=base)
        self.assertEqual(2, len(revs))
        self.assertEqual(['*', '+'], [x['text'][-1] for x in revs])

if __name__ == '__main__':
    unittest.main()
