/**
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */

package org.whym.growthring

import scala.jdk.CollectionConverters.*
import jakarta.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import jakarta.servlet.ServletConfig
import org.json4s.{JObject, JField, JArray, JValue, JInt}
import org.json4s.native.JsonMethods.*
import org.json4s.JsonDSL.*
import scala.io
import scala.util.{Failure, Success, Try}
import org.apache.commons.text.{StringEscapeUtils as seu}

/**
  * a servlet to receive a string and returns and visualizes repeated substrings
  * in it.
  *
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */
class FindRepeatsServlet extends HttpServlet {
  override def init(config: ServletConfig): Unit = {}

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse) =
    doGet(req, resp)

  override def doGet(
      req: HttpServletRequest,
      resp: HttpServletResponse
  ): Unit = {
    resp.setCharacterEncoding("UTF-8")

    val str = req.getParameter("q") match {
      case null => " "
      case ""   => " "
      case x    => x
    }
    val min_len = req.getParameter("min") match {
      case null => 1
      case x    => x.toInt
    }
    val threshold = req.getParameter("n") match {
      case null => IndexedSeq(2)
      case x    => x.split(",").map(_.toInt).filter(_ >= 2).sorted.toIndexedSeq
    }
    val attr = (req.getParameter("prop") match {
      case null => Array("plain", "max_repeats")
      case x    => x.split('|')
    }).map(_.toLowerCase).toSet

    val threshold_rev =
      threshold.zipWithIndex.map(x => (x._1, x._2 + 1)).toMap ++ Map((0, 0))
    val es = new ExtremalSubstrings(SuffixArrays.build(str, "jsuffixarrays"))

    case class Repeats(
        threshold: Int,
        regions: Seq[(Int, Int)],
        flags: Set[Int]
    )
    val repeats = threshold.map(x => {
      val rp = es.maxRepeats(x).filter(x => (x._2 - x._1 + 1) >= min_len)
      Repeats(x, rp, Covering.dp(str.toCharArray, rp, 0))
    })

    lazy val flags = Array.tabulate(str.length)(i => {
      repeats.foldLeft(Set[Int]())((s, x) =>
        if x.flags(i) then { s + x.threshold }
        else { s }
      )
    })

    lazy val max_flags = Array.tabulate(str.length)(i => {
      repeats.reverse.find(x => x.flags(i)) match {
        case Some(n) => n.threshold
        case None    => 0
      }
    })

    lazy val repeats_deepest = {
      val a = repeats.filter(_.regions.nonEmpty)
      if a.nonEmpty then {
        a.last
      } else {
        repeats.last
      }
    }

    lazy val masked_plain =
      str.zip(flags.map(_.contains(repeats_deepest.threshold))).map {
        case (char, true) => "" + char
        case (char, false) => (
          if 0x00 <= char && char <= 0xFF then { "_" }
          else { "__" }
        )
      }.mkString

    lazy val layers = repeats.map(rp =>
      (rp.threshold, TiledLayers.greedyTiling(str.toCharArray, rp.regions))
    )
    import org.whym.growthring.{TiledLayers as TL}
    val cell2char: TL.Cell => String = {
      case TL.Outside() => "O"
      case TL.Single()  => "S"
      case TL.Begin()   => "B"
      case TL.End()     => "E"
      case TL.Inside()  => "I"
    }

    lazy val layers_html = layers.map { x =>
      {
        val thres = x._1
        val layers_plain = x._2
        (
          thres,
          if layers_plain.size == 0 then { "" }
          else {
            val elements = layers_plain.map {
              s =>
                "<series>" + s.zipWithIndex.map(x =>
                  "<e class='" + cell2char(x._1) + "'>" + str.charAt(
                    x._2
                  ) + "</e>"
                ).mkString + "</series>\n"
            }.mkString
            "<set>" + elements + "</set>"
          }
        )
      }
    }

    def if_field(field: String, f: => JValue): JField = {
      if attr contains field then {
        JField(field, f)
      } else {
        JField(field, "")
      }
    }

    val writer = resp.getWriter
    req.getParameter("format") match {
      case "plain" => {
        resp.setContentType("text/plain")
        writer.print(masked_plain)
      }
      case _ => {
        resp.setContentType("application/json")
        val o =
          JObject(List(
            if_field("masked_plain", masked_plain),
            if_field(
              "chart",
              str.zip(flags).map(x =>
                Array.tabulate(threshold.length)(i =>
                  (if x._2(threshold(threshold.length - i - 1)) then { "*" }
                   else { " " })
                ).mkString + x._1
              ).mkString("\n") + "\n"
            ),
            if_field(
              "flags",
              JArray(flags.toList.map(x => JArray(x.toList.map(JInt(_)))))
            ),
            if_field("freqs", max_flags.toList.map(JInt(_))),
            if_field(
              "freqs_html",
              max_flags.zipWithIndex.map(x => {
                val c = str.charAt(x._2) match {
                  case ' ' => "&nbsp;"
                  case y   => y.toString
                }
                val newline = str.charAt(x._2) match {
                  case '\n' => " nl"
                  case _    => ""
                }
                f"<div class='cell c${threshold_rev(x._1)}$newline'>$c</div>"
              }).mkString("")
            ),
            if_field(
              "masked_html",
              str.zip(flags.map(_.contains(repeats_deepest.threshold))).map {
                case (char, true)  => char.toString
                case (char, false) => f"<del>$char</del>"
              }.mkString
            ),
            if_field(
              "layers",
              JObject(layers.map(x =>
                JField(
                  x._1.toString,
                  JArray(List[JValue](x._2.map(l => l.map(cell2char))))
                )
              ).toList)
            ),
            if_field(
              "layers_html",
              JObject(layers_html.map(x => JField(x._1.toString, x._2)).toList)
            ),
            if_field(
              "max_repeats",
              repeats.map { rp =>
                JArray(List[JValue](
                  rp.threshold,
                  rp.regions.map(x => JArray(List(x._1, x._2)))
                ))
              }
            )
          ))
        writer.println(pretty(render(o)))
      }
    }
  }
}

/**
  * a servlet to receive a MediaWiki page ID and returns and visualizes repeated
  * substrings in the edit history.
  *
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */
class WikiBlameServlet extends HttpServlet {

  override def init(config: ServletConfig): Unit = {}

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse) =
    doGet(req, resp)

  override def doGet(
      req: HttpServletRequest,
      resp: HttpServletResponse
  ): Unit = {
    resp.setCharacterEncoding("UTF-8")
    val title = req.getParameter("title")
    val ng = Option(req.getParameter("n")).getOrElse("30").toInt
    val mrevs = Option(req.getParameter("max")).getOrElse("100").toInt
    val base =
      Option(req.getParameter("base")).getOrElse("https://en.wikipedia.org/w")
    val revisions = WikiBlameServlet.getRevs(title, base, mrevs)
    if revisions.size == 0 then {
      resp.sendError(HttpServletResponse.SC_SEE_OTHER, "revision not found")
    } else {
      val html = WikiBlameServlet.getHtml(revisions, ng)
      // write response
      val writer = resp.getWriter
      resp.setContentType("application/json")
      writer.println(
        compact(render(
          JObject(List(
            JField("title", title),
            JField("nrevs", revisions.size),
            JField("rev_id", revisions(0).id),
            JField("timestamp", revisions(0).timestamp),
            JField("html", html)
          ))
        ))
      )
    }
  }
}

object WikiBlameServlet {
  case class VersionedString(
      timestamp: String,
      id: Int,
      body: String,
      depth: Int
  )

  def getSpans(
      revisions: Seq[VersionedString],
      n: Int
  ): Seq[VersionedString] = {
    val revs = revisions.map(_.body)
    val spans =
      NgramBlame.blameGreedy(revs(0), revs.slice(1, revs.size).toIndexedSeq, n)
    spans.toList.sorted(Ordering.by[(Int, Int, Int), Int](_._1)).map {
      case (s, e, i) =>
        VersionedString(
          revisions(i).timestamp,
          revisions(i).id,
          revs(0).slice(s, e),
          i
        )
    }
  }

  def getHtml(revisions: Seq[VersionedString], n: Int): String = {
    val revs = revisions.map(_.body)
    val spans =
      NgramBlame.blameGreedy(revs(0), revs.slice(1, revs.size).toIndexedSeq, n)
    val starts = spans.map(x => (x._1, x._3 + 1)).toMap
    val ends = spans.map(_._2).toSet

    val html = revs(0).zipWithIndex.map {
      case (c, i) => {
        (starts.get(i) match {
          case Some(x) =>
            f"""<span class="rev${x}%d" title="${revisions(
                x
              ).id}%d, ${revisions(x).timestamp}%s">""" + seu.escapeHtml4(
              c.toString
            )
          case _ => seu.escapeHtml4(c.toString)
        }) + (if ends.contains(i + 1) then {
                "</span>"
              } else {
                ""
              })
      }
    }.mkString("")
    html
  }

  def getRevs(
      title: String,
      base: String,
      maxRevs: Int
  ): Seq[VersionedString] = {
    import scala.xml.parsing.XhtmlParser
    val url =
      f"${base}%s/index.php?title=Special:Export&pages=${title}%s&history"
    System.out.println(url)
    Try(io.Source.fromURL(url)) match {
      case Success(src) =>
        if src.isEmpty then {
          System.err.println(f"${url}: empty response")
          Seq()
        } else {
          (XhtmlParser(src) \\ "revision").map {
            rev =>
              VersionedString(
                (rev \ "timestamp").text.toString,
                (rev \ "id").text.toInt,
                (rev \ "text").text.toString,
                -1
              )
          }.sorted(Ordering.by[VersionedString, String](_.timestamp)).reverse
        }
      case Failure(e) => {
        System.err.println(f"${url}: ${e}")
        Seq()
      }
    }
  }
}

/**
  * a servlet to visualize maximum repeats and minimum uniques
  *
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */
class NestedRepeatsServlet extends HttpServlet {

  override def init(config: ServletConfig): Unit = {}

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse) =
    doGet(req, resp)

  override def doGet(
      req: HttpServletRequest,
      resp: HttpServletResponse
  ): Unit = {
    resp.setCharacterEncoding("UTF-8")
    val body = req.getParameter("q")
    val boundary = req.getParameter("boundary")
    val min = Option(req.getParameter("min")).getOrElse("10").toInt
    val max = Option(req.getParameter("max")).getOrElse("10").toInt
    // write respones
    val writer = resp.getWriter
    resp.setContentType("application/json")
    writer.println(
      compact(render(
        JObject(List(
          JField("max", max)
        ))
      ))
    )
  }
}

object NestedRepeatsServlet {}
