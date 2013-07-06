/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import javax.servlet.ServletConfig
import org.json4s.{JObject, JField, JArray, JValue, JsonAST}
import org.json4s.native.{Printer, JsonMethods}
import org.json4s.JsonDSL._
import scala.io
import org.apache.commons.lang3.{StringEscapeUtils => seu}

/**
 * a servlet to receive a string and returns and visualizes repeated substrings in it.
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
      Repeats(x, rp, Covering.greedyLengthFreq(str.toCharArray, rp))
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
    // abstract sealed class Tag(){}
    // case class Begin(threshold: Int, s: String) extends Tag
    // case class End(threshold: Int, s: String) extends Tag
    // case object NoTag extends Tag
    // for ( r <- repeats_deepest.regions ) {
    //   tags(r._1)     = Begin(repeats_deepest.threshold, str.slice(r._1, r._2 + 1))
    //   tags(r._2 + 1) = End(repeats_deepest.threshold, str.slice(r._1, r._2 + 1))
    // }
    val masked_html = str.zip(flags.map(_.contains(repeats_deepest.threshold))).map{
      case (char, true)  => char.toString
      case (char, false) => f"<del>${char}</del>"
    }.reduce(_+_)

    val masked_plain = str.zip(flags.map(_.contains(repeats_deepest.threshold))).map{
      case (char, true)  => "" + char
      case (char, false) => (if (0x00 <= char && char <= 0xFF) {"_"} else {"__"})
    }.reduce(_+_)

    val layers_plain = TiledLayers.greedyTiling(str.toCharArray, repeats_deepest.regions)
    import org.whym.growthring.{TiledLayers => TL}
    val cell2char: TL.Cell => String = {
        case TL.Outside() => "O"
        case TL.Single()  => "S"
        case TL.Begin()   => "B"
        case TL.End()     => "E"
        case TL.Inside()  => "I"
    }
    val layers_html = if (layers_plain.size == 0) {""} else {
      layers_plain.map{
        s => "<tr>" + s.map(x => "<td>" + cell2char(x) + "</td>").mkString + "</tr>"
      }.reduce(_+_)
    }
    val writer = resp.getWriter
    req.getParameter("format") match {
      case "plain" => {
        resp.setContentType("text/plain")
        writer.print(masked_plain)
      }
      case _ => {
        resp.setContentType("application/json")
        writer.println(
          Printer.pretty(JsonMethods.render(
            JObject(List(JField("plain", masked_plain),
                         JField("chart", chart),
                         JField("html", masked_html),
                         JField("layers", JArray(List[JValue](layers_plain.map(l => l.map(cell2char))))),
                         JField("layers_html", layers_html),
                         JField("max_repeats",
                                repeats.map{rp => JArray(List[JValue](
                                  rp.threshold,
                                  rp.regions.map(x => JArray(List(x._1, x._2)))
                                ))}))))))
      }
    }
  }
}


/**
 * a servlet to receive a string and returns and visualizes repeated substrings in it.
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
class WikiBlameServlet extends HttpServlet {

  override def init(config: ServletConfig) {
  }

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse) = doGet(req, resp)

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    resp.setCharacterEncoding("UTF-8")
    val title = req.getParameter("title")
    val ng    = Option(req.getParameter("n")).getOrElse("30").toInt
    val mrevs = Option(req.getParameter("max")).getOrElse("100").toInt
    val base  = Option(req.getParameter("base")).getOrElse("http://en.wikipedia.org/w")
    val revisions  = WikiBlameServlet.getRevs(title, base, mrevs)
    val html = WikiBlameServlet.getHtml(revisions, ng)
    // write response
    val writer = resp.getWriter
    resp.setContentType("application/json")
    writer.println(
      Printer.pretty(JsonMethods.render(
        JObject(List(JField("title", title),
                     JField("nrevs", revisions.size),
                     JField("rev_id", revisions(0).id),
                     JField("timestamp", revisions(0).timestamp),
                     JField("html", html))))))
  }
}

object WikiBlameServlet {
  case class Revision(timestamp: String, id: Int, body: String)

  def getHtml(revisions: Seq[Revision], n: Int) = {
    val revs = revisions.map(_.body)
    val spans = NgramBlame.blameGreedy(revs(0), revs.slice(1, revs.size).toIndexedSeq, n)
    val starts = spans.map(x => (x._1, x._3+1)).toMap
    val ends   = spans.map(_._2).toSet

    // System.err.println("revs: " + revs) //!
    // System.err.println("spans: " + spans) //!
    // System.err.println("starts: " + starts) //!
    // System.err.println("ends: " + ends) //!

    val html = revs(0).zipWithIndex.map{
      case (c,i) => {
        (starts.get(i) match {
          case Some(x) => f"""<span class="rev${x}%d" title="${revisions(x).id}%d, ${revisions(x).timestamp}%s">""" + seu.escapeHtml4(c.toString)
          case _ => seu.escapeHtml4(c.toString)
        }) + (if (ends.contains(i+1)) {
          "</span>"
        } else {
          ""
        })
      }
    }.mkString("")
    html
  }

  def getRevs(title: String, base: String, maxRevs: Int): Seq[Revision] = {
    import scala.xml.parsing.XhtmlParser
    val url = f"${base}%s/index.php?title=Special:Export&pages=${title}%s&history"
    (XhtmlParser(io.Source.fromURL(url)) \\ "revision").map{
      rev => Revision((rev \ "timestamp").text.toString, (rev \ "id").text.toInt, (rev \ "text").text.toString)
    }.sorted(Ordering.by[Revision,String](_.timestamp)).reverse
  }
}

