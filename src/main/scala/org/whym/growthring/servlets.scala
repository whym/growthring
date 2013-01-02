/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import javax.servlet.{ServletConfig}

/**
 * servlet to receive a pair of strings and returns repeating substrings.
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
class FindRepeatsServlet extends HttpServlet {
  override def init(config: ServletConfig) {
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    val writer = resp.getWriter
    val str = req.getParameter("q") match {
      case null => " "
      case ""   => " "
      case x => x
    }
    val threshold = req.getParameter("n") match {
      case null => 2
      case x => x.toInt
    }
    val es = new ExtremalSubstrings(str)

    resp.setContentType("text/plain")
    resp.setCharacterEncoding("UTF-8")
    req.getParameter("format") match {
      case "raw" => {
        writer.println(es.maxRepeats(threshold).map(x => "(" + x._1 + ", " + x._2 + ")").mkString("\n"))
      }
      case _ => {
        writer.printf("{\"max_repeats\": [%s]}\n",
                      es.maxRepeats(threshold).map(x => "[" + x._1 + ", " + x._2 + "]").mkString(", "))
      }
    }
  }
}
