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

  def anonymize(rmethod: (String,Int)=>Seq[(Int,Int)],
                cmethod: (Array[Char], Seq[(Int,Int)]) => Set[Int],
                strings: Seq[String],
                min_len: Int, cover_char: Char, freq: Int, unhide_pattern: String=""): Seq[String] = {
    import scala.util.matching.Regex
    val str = strings.mkString("\n")
    logger.debug(f"${str.size}%d characters, frequency at least ${freq}%d, each unprotected span at least ${min_len}%d in length.")
    val covered = for ( (s,e) <- rmethod(str, freq); if e - s + 1 >= min_len ) yield (s,e)

    logger.debug(f"${covered.size}%d repeats.")
    val flags = cmethod(str.toCharArray, covered)
    logger.debug(f"${flags.size} characters unsuppressed.")
    import scala.collection.mutable
    val unhides = new mutable.HashSet[Int]
    for ( x <- (new Regex(unhide_pattern) findAllMatchIn str) ) {
      for ( i <- Range(x.start,x.end) ) {
        unhides(i) = true
      }
    }
    logger.debug(f"${unhides.size} matched to regex ${unhide_pattern}.")
    val mflags = flags ++ unhides
    Seq(str.zip(Array.tabulate(str.length)(i => mflags(i))).map(_ match {case (c,true) => c; case (c,false) => cover_char}).mkString)
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
    import com.typesafe.config.ConfigFactory
    val config = ConfigFactory.load()

    logger.info("**** main begin ****")
    import scala.io
    import scala.xml.parsing.XhtmlParser

    val strings = (if (args.length > 0) {
      args.map(io.Source.fromFile(_).getLines.toList).flatMap(x => x).toList
    } else {
      io.Source.fromInputStream(System.in).getLines.toList
    })
    logger.debug(f"${strings.size}%d lines.")

    import scala.sys
    sys.addShutdownHook {
      logger.info("**** shutdown ****")
    }

    config.getString("mode") match {
      case "multiple-anonym" =>
        logger.debug("multiple-anonym is not implemented")
        for ( config <- XhtmlParser(io.Source.fromFile(config.getString("configFile"))) \\ "config" ) {
          println((config \ "file").text)
        }
      case "anonym" => {
        val rmethodstr = config.getString("repeatsMethod")
        val cmethodstr = config.getString("coveringMethod")
        logger.debug("repeats type '${rmethodstr}'")
        logger.debug("covering type '${cmethodstr}'")
        val rmethod: (String,Int)=>Seq[(Int,Int)] = config.getString("repeatsMethod") match {
          case "naive" =>  NaiveExtremalSubstrings.maxRepeats
          case "word"  =>  new WordRepeats().repeats
          case "ngram" =>  new NgramRepeats(config.getInt("ngramSize")).repeats(_, _, config.getInt("minLen").toInt)
          case x =>   new ExtremalSubstrings(_, x).maxRepeats(_)
        }

        val cmethod: (Array[Char],Seq[(Int,Int)])=>Set[Int] = config.getString("coveringMethod") match {
          case "greedyLength" =>       Covering.greedyLength
          case "greedyConservative" => Covering.greedyConservative
          case "greedySliced" =>       Covering.greedySliced
          case _        =>             {
            logger.debug("using default covering algorithm")
            Covering.greedyLengthFreq
          }
        }
        for ( s <- anonymize(rmethod,
                             cmethod,
                             strings,
                             config.getInt("minLen").toInt,
                             config.getString("coverChar")(0),
                             config.getInt("repeats").toInt,
                             config.getString("unhide")) ) {
          println(s)
        }
      }
      case "repeats" =>
        for ( s <- findRepeats(config.getString("repeatsMethod"),
                               strings,
                               config.getInt("repeats").toInt) ) {
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
