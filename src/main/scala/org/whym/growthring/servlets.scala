/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import javax.servlet.ServletConfig
import net.liftweb.json.{JObject, JField, JArray, JInt, JValue, JString, JsonAST, Printer}
import net.liftweb.json.JsonDSL._

/**
 * servlet to receive a pair of strings and returns repeating substrings.
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
class FindRepeatsServlet extends HttpServlet {
  override def init(config: ServletConfig) {
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    resp.setCharacterEncoding("UTF-8")

    val str = req.getParameter("q") match {
      case null => " "
      case ""   => " "
      case x => x
    }
    val threshold: Seq[Int] = req.getParameter("n") match {
      case null => Seq(2, 4, 8)
      case x => x.split(",").map(_.toInt)
    }
    val es = new ExtremalSubstrings(str)

    case class Repeats(threshold: Int, regions: Seq[(Int, Int)], flags: Set[Int])
    val repeats = threshold.map(x => {
      val rp = es.maxRepeats(x)
      val s = rp.map(x => (x._1 to x._2).toList).foldLeft(Set[Int]())(_++_).toSet
      Repeats(x, rp, s)
    })
    val array = Array.tabulate(str.length)(i => {
      repeats.foldLeft(Set[Int]())((s,x) => if ( x.flags(i) ){s + x.threshold} else {s})
    })
    val raw = str.zip(array).map(
      x =>
        Array.tabulate(threshold.length)(
          i => (if (x._2(threshold(threshold.length - i - 1))){"*"}else{" "})
        ).mkString + x._1
    ).mkString("\n") + "\n"

    val writer = resp.getWriter
    req.getParameter("format") match {
      case "raw" => {
        resp.setContentType("text/plain")
        writer.print(raw)
      }
      case _ => {
        resp.setContentType("application/json")
        writer.println(
          Printer.pretty(JsonAST.render(
            JObject(List(JField("raw", raw),
                         JField("max_repeats",
                                threshold.map(t =>
                                  JArray(List[JValue](
                                    JInt(t),
                                    es.maxRepeats(t).map(
                                      x => JArray(List(JInt(x._1), JInt(x._2)))))))))))))
      }
    }
  }
}
