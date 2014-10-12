/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

import org.whym.growthring._
import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import java.io.{Writer, PrintWriter, StringWriter}
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.json4s.{JInt, JString, JField, JArray, JValue}
import org.json4s.native.JsonParser._
import org.json4s.JsonDSL._

/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
class TestFindRepeatsServlet extends FunSuite with MockitoSugar {
  def get(input: String, n: String, min: String): JValue = {
    val response = mock[HttpServletResponse]
    val request = mock[HttpServletRequest]
    val stringWriter = new StringWriter
    val printWriter = new PrintWriter(stringWriter)
    
    when(response.getWriter()).thenReturn(printWriter)
    when(request.getParameter("q")).thenReturn(input)
    when(request.getParameter("n")).thenReturn(n)
    when(request.getParameter("min")).thenReturn(min)
    when(request.getParameter("format")).thenReturn("json")
    when(request.getParameter("prop")).thenReturn("masked_plain|masked_html|chart|flags|freqs|freqs_html|layers|layers_html|max_repeats")
    
    new FindRepeatsServlet().doGet(request, response)
    return parse(stringWriter.toString)
  }
  val json = get("bananA wanapa", "2,4,8", "1")
  val json2 = get("abracadabra", "2", "2")
  test("find repeats (plain)") {

    assertResult(JString("_a_a____a_a_a")) {
      json \ "masked_plain"
    }
  }

  test("find repeats (chart)") {
    assertResult(JString("""   b
 **a
  *n
 **a
   n
   A
    
   w
 **a
  *n
 **a
   p
 **a
""")) {
      json \ "chart"
    }
  }

  test("find repeats (freqs)") {
    assertResult(JArray(List(JInt(0),
                             JInt(4),
                             JInt(2),
                             JInt(4),
                             JInt(0),
                             JInt(0),
                             JInt(0),
                             JInt(0),
                             JInt(4),
                             JInt(2),
                             JInt(4),
                             JInt(0),
                             JInt(4)))) {
      json \ "freqs"
    }
  }

  test("find repeats (freqs html)") {
    assertResult(JString("<div class='cell c0'>b</div><div class='cell c2'>a</div><div class='cell c1'>n</div><div class='cell c2'>a</div><div class='cell c0'>n</div><div class='cell c0'>A</div><div class='cell c0'>&nbsp;</div><div class='cell c0'>w</div><div class='cell c2'>a</div><div class='cell c1'>n</div><div class='cell c2'>a</div><div class='cell c0'>p</div><div class='cell c2'>a</div>")) {
      json \ "freqs_html"
    }
  }

  test("find repeats (list)"){
    assertResult(JArray(List(JArray(List(2,
                                         List(List(1, 3),
                                              List(3, 4),
                                              List(8, 10),
                                              List(12, 12)))),
                             JArray(List(4,
                                         List(List(1, 1),
                                              List(3, 3),
                                              List(8, 8),
                                              List(10, 10),
                                              List(12, 12)))),
                             JArray(List((8),
                                         JArray(List())))))) {
      json \ "max_repeats"
    }
  }

  test("find repeats (plain#2)") {
    assertResult(JString("abra___abra")) {
      json2 \ "masked_plain"
    }
    assertResult(JArray(List(JArray(List(2,
                                         List(List(0, 3),
                                              List(7, 10))))))) {
      json2 \ "max_repeats"
    }
  }

  test("find repeats (masked_html)") {
    assertResult(JString("abra<del>c</del><del>a</del><del>d</del>abra")) {
      json2 \ "masked_html"
    }
  }

  test("find repeats (layers)"){
    assertResult(JArray(List(JArray(List(JArray(List("B", "I", "I", "E", "O", "O", "O", "B", "I", "I", "E"))))))) {
      json2 \ "layers"
    }
  }
  
  test("find repeats (layers html)"){
    assertResult(JString("<tr><td>B</td><td>I</td><td>I</td><td>E</td><td>O</td><td>O</td><td>O</td><td>B</td><td>I</td><td>I</td><td>E</td></tr>")) {
      json2 \ "layers_html"
    }
  }

}

class TestWikiBlameServlet extends FunSuite with MockitoSugar {
  def get(): JValue = {
    val response = mock[HttpServletResponse]
    val request = mock[HttpServletRequest]
    val stringWriter = new StringWriter
    val printWriter = new PrintWriter(stringWriter)
    
    val addr = SimpleHttpServer.findFreeAddress()
    val port = addr.getPort

    when(response.getWriter()).thenReturn(printWriter)
    when(request.getParameter("base")).thenReturn("http://localhost:" + port)
    when(request.getParameter("title")).thenReturn("Main_page")
    when(request.getParameter("n")).thenReturn("3")
    
    SimpleHttpServer.create("localhost", port,
                            Map(("/index.php", <mediawiki xmlns="http://www.mediawiki.org/xml/export-0.8/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http:
//www.mediawiki.org/xml/export-0.8/ http://www.mediawiki.org/xml/export-0.8.xsd" version="0.8" xml:lang="ja">
  <siteinfo>
    <sitename>MyLocalWiki</sitename>
    <base></base>
    <generator>MediaWiki 1.22alpha</generator>
    <case>case-sensitive</case>
    <namespaces>
    </namespaces>
  </siteinfo>
  <page>
    <title>Main Page</title>
    <ns>0</ns>
    <id>1</id>
    <revision>
      <id>1200</id>
      <parentid>1199</parentid>
      <timestamp>2013-06-04T12:00:00Z</timestamp>
      <contributor>
        <ip>0:0:0:0:0:0:0:1</ip>
      </contributor>
      <text xml:space="preserve" bytes="6">aaaccc</text>
      <sha1></sha1>
      <model>wikitext</model>
      <format>text/x-wiki</format>
    </revision>
    <revision>
      <id>1201</id>
      <parentid>1200</parentid>
      <timestamp>2013-06-04T12:10:00Z</timestamp>
      <contributor>
        <ip>0:0:0:0:0:0:0:1</ip>
      </contributor>
      <text xml:space="preserve" bytes="6">aaabbb</text>
      <sha1></sha1>
      <model>wikitext</model>
      <format>text/x-wiki</format>
    </revision>
    <revision>
      <id>1202</id>
      <parentid>1201</parentid>
      <timestamp>2013-06-04T12:20:00Z</timestamp>
      <contributor>
        <ip>0:0:0:0:0:0:0:1</ip>
      </contributor>
      <text xml:space="preserve" bytes="7">bbb&lt;ccc</text>
      <sha1></sha1>
      <model>wikitext</model>
      <format>text/x-wiki</format>
    </revision>
  </page>
</mediawiki>.toString))).start
    TestSimpleHttpServer.waitUntilPrepared(addr, 1000L)
    new WikiBlameServlet().doGet(request, response)
    return parse(stringWriter.toString)
  }
  val json = get()

  test("wiki blame servlet") {
    assertResult(JString("Main_page")) {
      json \ "title"
    }
    assertResult(JInt(3)) {
      json \ "nrevs"
    }
    assertResult(JInt(1202)) {
      json \ "rev_id"
    }
    assertResult(JString("""<span class="rev1" title="1201, 2013-06-04T12:10:00Z">bbb</span>&lt;<span class="rev2" title="1200, 2013-06-04T12:00:00Z">ccc</span>""")) {
      json \ "html"
    }
  }

  test("wiki blame spans") {
    import WikiBlameServlet._
    assertResult(List(VersionedString("2011", 20, "aaa", 1),
                      VersionedString("2010", 10, "ccc", 0))) {
      getSpans(Seq(VersionedString("2010", 10, "aaaccc", 0),
                   VersionedString("2011", 20, "bbbccc", 1),
                   VersionedString("2012", 30, "aaabbb", 2)), 3)
    }
  }

}
