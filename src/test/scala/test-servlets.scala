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

/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
class TestFindRepeatsServlet extends FunSuite with MockitoSugar {
  test("find repeats (chart)") {
    expect("""   b
 **a
  *n
 **a
  *n
 **a
    
   w
 **a
  *n
 **a
""") {
      val response = mock[HttpServletResponse]
      val request = mock[HttpServletRequest]
      val stringWriter = new StringWriter
      val printWriter = new PrintWriter(stringWriter)
      
      when(response.getWriter()).thenReturn(printWriter)
      when(request.getParameter("q")).thenReturn("banana wana")
      when(request.getParameter("n")).thenReturn("2,4,8")
      when(request.getParameter("format")).thenReturn("plain")
      
      new FindRepeatsServlet().doGet(request, response)
      stringWriter.toString
    }
  }
}
