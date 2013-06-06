/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import java.io.{Writer, PrintWriter, StringWriter}
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import net.liftweb.json.{JsonParser, JString, JField, JArray, JValue}
import net.liftweb.json.JsonDSL._

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
    
    new FindRepeatsServlet().doGet(request, response)
    return JsonParser.parse(stringWriter.toString)
  }
  val json = get("bananA wanapa", "2,4,8", "1")
  val json2 = get("abracadabra", "2", "2")
  test("find repeats (plain)") {

    expectResult(JString("_a_a____a_a_a")) {
      json \ "plain"
    }
  }

  test("find repeats (chart)") {
    expectResult(JString("""   b
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

  test("find repeats (list)"){
    expectResult(JArray(List(JArray(List(2,
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
    expectResult(JString("abra___abra")) {
      json2 \ "plain"
    }
    expectResult(JArray(List(JArray(List(2,
                                         List(List(0, 3),
                                              List(7, 10))))))) {
      json2 \ "max_repeats"
    }
  }

  test("find repeats (html)") {
    expectResult(JString("abra<del>c</del><del>a</del><del>d</del>abra")) {
      json2 \ "html"
    }
  }

  test("find repeats (layers)"){
    expectResult(JArray(List(JArray(List(JArray(List("B", "I", "I", "E", "O", "O", "O", "B", "I", "I", "E"))))))) {
      json2 \ "layers"
    }
  }
  
  test("find repeats (layers html)"){
    expectResult(JString("<tr><td>B</td><td>I</td><td>I</td><td>E</td><td>O</td><td>O</td><td>O</td><td>B</td><td>I</td><td>I</td><td>E</td></tr>")) {
      json2 \ "layers_html"
    }
  }

}
