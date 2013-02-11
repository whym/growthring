/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring
import com.typesafe.scalalogging.slf4j.Logging

/**
 * Main entry point
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object Main extends Logging {

  def anonymize(method: String, strings: Seq[String], min_len: Int, cover_char: Char, freq: Int, covering: String): Seq[String] = {
    //! 改行はデフォルトで強制表示
    //! -Dunhide で指定したパターンは強制表示
    val str = strings.mkString("\n")
    logger.debug(f"${str.size}%d characters, ${method}, frequency at least ${freq}%d, each unprotected span at least ${min_len}%d in length, covering type '${covering}'.")
    val covered = method match {
      case "naive" => {
        import org.whym.growthring.{NaiveExtremalSubstrings => NES}
        for ( (s, e) <- NES.maxRepeats(str, freq)
             if e - s >= min_len ) yield (s, e)
      }
      case x => {
        val es = new ExtremalSubstrings(str, method)
        for ( (s, e) <- es.maxRepeats(freq)
             if e - s >= min_len ) yield (s, e)
      }
    }
    logger.debug(f"${covered.size}%d coverings.")
    val flags = covering match {
      case "greedy" =>             Covering.greedy(str.toCharArray, covered)
      case "greedyConservative" => Covering.greedyConservative(str.toCharArray, covered)
      case _        =>             Covering.greedySliced(str.toCharArray, covered)
        }
    
    Seq(str.zip(Array.tabulate(str.length)(i => flags(i))).map(_ match {case (c,true) => c; case (c,false) => cover_char}).mkString)
  }

  def findRepeats(method: String, strings: Seq[String], freq: Int): Seq[String] = {
    import org.apache.commons.lang3.{StringEscapeUtils => SEU}
    val str = strings.mkString("\n")
    val es = new ExtremalSubstrings(str, method)
    es.maxRepeats(freq).map{
      x =>
        f"r ${x._1}%d ${x._2}%d ${SEU.escapeJava(new String(str.slice(x._1, x._2 + 1)))}%s"
    } ++ es.minUniques.map{
      x =>
        f"u ${x._1}%d ${x._2}%d ${SEU.escapeJava(new String(str.slice(x._1, x._2 + 1)))}%s"
    }
  }

  def main(args: Array[String]) {
    logger.info("**** main begin ****")
    import scala.io
    import scala.util.Properties
    import scala.xml.parsing.XhtmlParser

    val strings = (if (args.length > 0) {
      args.map(io.Source.fromFile(_).getLines.toList).flatMap(x => x).toList
    } else {
      io.Source.fromInputStream(System.in).getLines.toList
    })
    logger.debug(f"${strings.size}%d lines.")

    Properties.propOrElse("mode", "anonym") match {
      case "multiple-anonym" =>
        for ( config <- XhtmlParser(io.Source.fromFile(Properties.propOrElse("configFile", "config.xml"))) \\ "config" ) {
          println((config \ "file").text)
        }
      case "anonym" =>
        for ( s <- anonymize(Properties.propOrElse("sufMethod", "jsuffixarrays"),
                             strings,
                             Properties.propOrElse("minLen", "2").toInt,
                             Properties.propOrElse("coverChar", "_")(0),
                             Properties.propOrElse("repeats", "2").toInt,
                             Properties.propOrElse("cover", "greedySliced")) ) {
          println(s)
        }
      case "repeats" =>
        for ( s <- findRepeats(Properties.propOrElse("sufMethod", "jsuffixarrays"),
                               strings, Properties.propOrElse("repeats", "2").toInt) ) {
          println(s)
        }
      case _ => {
        val msa = new MultipleSequenceAlignment[Char](strings.map(x => ("^"+x+"$").toCharArray.toIndexedSeq))
        val dag = msa.align.compact((x,y) => x.concat(y))
        def nodeformat(i: Int, x: MultipleSequenceAlignment.Node[Char]): String = {
            def escape(x: String) = x.replace("\\","\\\\").replace("\"", "\\\"")
          "  N_%d[label=\"%s\",fontsize=%f];".format(i, escape(x.label.map(_.toString).mkString),
                                                     10 + scala.math.log(x.freq) * 4)
          }
        for ( line <- dag.dot(nodeformat) ) {
          println(line)
        }
      }
    }
    logger.info("**** main end   ****")
  }
}
