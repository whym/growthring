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
import net.liftweb.json.{JsonParser, JString, JField, JArray}
import net.liftweb.json.JsonDSL._

/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
class TestFindRepeatsServlet extends FunSuite with MockitoSugar {
  test("find repeats (chart)") {
    val response = mock[HttpServletResponse]
    val request = mock[HttpServletRequest]
    val stringWriter = new StringWriter
    val printWriter = new PrintWriter(stringWriter)
    
    when(response.getWriter()).thenReturn(printWriter)
    when(request.getParameter("q")).thenReturn("banana wana")
    when(request.getParameter("n")).thenReturn("2,4,8")
    when(request.getParameter("format")).thenReturn("json")
    
    new FindRepeatsServlet().doGet(request, response)
    val json = JsonParser.parse(stringWriter.toString)

    expect(JString("_a_a_a__a_a")) {
      json \ "plain"
    }
    expect(JString("""   b
 **a
  *n
 **a
  *n
 **a
    
   w
 **a
  *n
 **a
""")) {
      json \ "chart"
    }
    expect(JArray(List(JArray(List(2,
                                   List(List(1, 3),
                                        List(3, 5),
                                        List(8, 10)))),
                       JArray(List(4,
                                   List(List(1, 1),
                                        List(3, 3),
                                        List(5, 5),
                                        List(8, 8),
                                        List(10, 10)))),
                       JArray(List((8),
                                   JArray(List())))))) {
      json \ "max_repeats"
    }
  }
}
