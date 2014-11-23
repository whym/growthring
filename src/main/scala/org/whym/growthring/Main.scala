/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring
import scala.collection.JavaConverters._
import com.typesafe.scalalogging.slf4j.Logging
import scala.collection.mutable

/**
 * Main entry point
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object Main extends Logging {

  def anonymize(rmethod: (String,Int)=>Seq[(Int,Int)],
                cmethod: (Array[Char], Seq[(Int,Int)], Int) => Set[Int],
                strings: Seq[String],
                min_len: Int,
                cover_char: Char,
                freq: Int,
                unhide_pattern: String="",
                gap: Int,
                _start: Int,
                _end: Int): Seq[String] = {
    import scala.util.matching.Regex
    val str = strings.mkString("\n")
    logger.debug(f"${str.size}%d characters, frequency at least ${freq}%d, each unprotected span at least ${min_len}%d in length.")
    val covered = for ( (s,e) <- rmethod(str, freq); if e - s + 1 >= min_len ) yield (s,e)

    val start = if (_start >= 0) {_start} else {0}
    val end = if (_end > 0) {_end} else {str.length}
    val coveredFiltered = covered.filter(x => (start <= x._1  && x._1 < end) || (start <= x._2 && x._2 < end))
    logger.debug(f"${coveredFiltered.size}%d / ${covered.size}%d repeats from ${start}%d to ${end}%d.")
    val flags = cmethod(str.toCharArray, coveredFiltered, gap)
    logger.debug(f"${flags.size} characters unsuppressed.")
    val unhides = new mutable.BitSet
    for ( x <- (new Regex(unhide_pattern) findAllMatchIn str) ) {
      for ( i <- Range(x.start,x.end) ) {
        unhides(i) = true
      }
    }
    logger.debug(f"${unhides.size} matched to regex ${unhide_pattern}.")
    val mflags = flags ++ unhides
    str.zip(Array.tabulate(str.length)(i => mflags(i))).map(_ match {case (c,true) => c; case (c,false) => cover_char}).mkString.slice(start, end).split('\n').toSeq
  }

  def formatSpan(str: String, x: (Int,Int)): String = {
    import org.apache.commons.lang3.{StringEscapeUtils => SEU}
    val f= str.slice(x._1, x._2 + 1).replace("\n", " ")
    f"${x._1}%d\t${x._2}%d\t${SEU.escapeJava(new String(str.slice(x._1, x._2 + 1)))}%s\t${f}%s"
  }

  def findRepeats(method: String, strings: Seq[String], freq: Int): Seq[String] = {
    val str = strings.mkString("\n")
    val es = new ExtremalSubstrings(SuffixArrays.build(str, method))
    es.maxRepeats(freq).map{
      x => {
        f"r " + formatSpan(str, x)
      }
    } ++ es.minUniques.map{
      x =>
        f"u " + formatSpan(str, x)
    }
  }

  def main(args: Array[String]) {
    import com.typesafe.config.ConfigFactory
    val config = ConfigFactory.load.getConfig("org.whym.growthring")

    logger.info("**** main begin ****")
    logger.info("config = " + config.entrySet.asScala.mkString("\t"))
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
        logger.debug(f"repeats type '${rmethodstr}'")
        logger.debug(f"covering type '${cmethodstr}'")
        val rmethod: (String,Int)=>Seq[(Int,Int)] = config.getString("repeatsMethod") match {
          case "naive" =>  NaiveExtremalSubstrings.maxRepeats
          case "word"  =>  new WordRepeats().repeats
          case "ngram" =>  new NgramRepeats(config.getInt("ngramSize")).repeats(_, _, config.getInt("minLen").toInt)
          case m => {(s:String, r:Int) => new ExtremalSubstrings(SuffixArrays.build(s, m)).maxRepeats(r)}
        }

        val cmethod: (Array[Char],Seq[(Int,Int)],Int)=>Set[Int] = config.getString("coveringMethod") match {
          case "greedyLength" =>  Covering.greedyLength
          //case "greedyConservative" => Covering.greedyConservative
          case "greedySliced" =>  Covering.greedySliced
          case "exhaustive" =>    Covering.exhaustive
          case "dp" =>            Covering.dp
          case x  => {
            logger.debug("\"" + x + "\" not found; using default covering algorithm")
            Covering.greedyLengthFreq
          }
        }
        for ( s <- anonymize(rmethod,
                             cmethod,
                             strings,
                             config.getInt("minLen").toInt,
                             config.getString("coverChar")(0),
                             config.getInt("repeats").toInt,
                             config.getString("unhide"),
                             config.getInt("gap"),
                             config.getInt("start"),
                             config.getInt("end")) ) {
          println(s)
        }
      }
      case "msa" => {
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
      case "segment" => {
        val boundaries = new mutable.ArrayBuffer[Int]
        boundaries.append(0)
        for (s <- strings) {
          boundaries.append(boundaries.last + s.length)
        }
        val str = strings.mkString
        val bd = Array.fill(str.size + 1)(str.size)
        var i = 0
        for ( b <- boundaries ) {
          while ( i < b ) {
            bd(i) = b
            i += 1
          }
        }
        val es = new ExtremalSubstrings(SuffixArrays.build(str, config.getString("repeatsMethod")))
        val rps = es.maxRepeats(config.getInt("repeats").toInt, bd)
        for ( x <- rps ) {
          println(formatSpan(str, x))
        }
      }
      case name @ ("repeats" | _) => {
        if ( name != "repeats" ) {
          logger.debug("\"" + name + "\" not found; using default mode 'repeats'")
        }
        for ( s <- findRepeats(config.getString("repeatsMethod"),
                               strings,
                               config.getInt("repeats").toInt) ) {
          println(s)
        }
      }
    }
    logger.info("**** main end   ****")
  }
}
