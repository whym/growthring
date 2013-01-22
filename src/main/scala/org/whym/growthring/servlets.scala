/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import javax.servlet.ServletConfig
import net.liftweb.json.{JObject, JField, JArray, JValue, JsonAST, Printer}
import net.liftweb.json.JsonDSL._

/**
 * servlet to receive a pair of strings and returns repeating substrings.
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
class FindRepeatsServlet extends HttpServlet {
  override def init(config: ServletConfig) {
  }

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse) = doGet(req, resp)

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    resp.setCharacterEncoding("UTF-8")

    val str = req.getParameter("q") match {
      case null => " "
      case ""   => " "
      case x => x
    }
    val min_len = req.getParameter("min") match {
      case null => 1
      case x => x.toInt
    }
    val threshold: Seq[Int] = req.getParameter("n") match {
      case null => Seq(2)
      case x => x.split(",").map(_.toInt).filter(_ >= 2).sorted
    }
    val es = new ExtremalSubstrings(str)

    case class Repeats(threshold: Int, regions: Seq[(Int, Int)], flags: Set[Int])
    val repeats = threshold.map(x => {
      val rp = es.maxRepeats(x).filter(x => (x._2 - x._1 + 1) >= min_len)
      Repeats(x, rp, ExtremalSubstrings.coveredCells(rp))
    })
    val flags = Array.tabulate(str.length)(i => {
      repeats.foldLeft(Set[Int]())((s,x) => if ( x.flags(i) ){s + x.threshold} else {s})
    })
    val chart = str.zip(flags).map(
      x =>
        Array.tabulate(threshold.length)(
          i => (if (x._2(threshold(threshold.length - i - 1))){"*"}else{" "})
        ).mkString + x._1
    ).mkString("\n") + "\n"

    val repeats_deepest = {
      val a = repeats.filter(_.regions.length > 0)
      if ( a.length > 0 ) {
        a.last
      } else {
        repeats.last
      }
    }
    abstract sealed class Tag(){}
    case class Begin(threshold: Int, s: String) extends Tag
    case class End(threshold: Int, s: String) extends Tag
    case object NoTag extends Tag
    val tags: Array[Tag] = Array.fill(str.length + 1)(NoTag)
    for ( r <- repeats_deepest.regions ) {
      tags(r._1)     = Begin(repeats_deepest.threshold, str.slice(r._1, r._2 + 1))
      tags(r._2 + 1) = End(repeats_deepest.threshold, str.slice(r._1, r._2 + 1))
    }
    val masked_html = str.zip(tags).map{
      case (char, Begin(threshold, label)) =>
        "<span class=\"R" + threshold + "\">" + char
      case (char, End(threshold, label)) =>
         "</span>" + char
      case (char, NoTag) =>
        "" + char
    }.reduce(_+_)
    val masked_plain = str.zip(flags.map(_.contains(repeats_deepest.threshold))).map{
      case (char, true)  => "" + char
      case (char, false) => (if (0x00 <= char && char <= 0xFF) {"_"} else {"__"})
    }.reduce(_+_)

    val writer = resp.getWriter
    req.getParameter("format") match {
      case "plain" => {
        resp.setContentType("text/plain")
        writer.print(masked_plain)
      }
      case _ => {
        resp.setContentType("application/json")
        writer.println(
          Printer.pretty(JsonAST.render(
            JObject(List(JField("plain", masked_plain),
                         JField("chart", chart),
                         JField("html", masked_html),
                         JField("max_repeats",
                                repeats.map{rp => JArray(List[JValue](
                                  rp.threshold,
                                  rp.regions.map(x => JArray(List(x._1, x._2)))
                                ))}))))))
      }
    }
  }
}
