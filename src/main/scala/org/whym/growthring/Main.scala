/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring
import scala.collection.JavaConverters._
import com.typesafe.scalalogging.slf4j.Logging
import scala.collection.mutable
import scala.util.matching.Regex

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

  def findBoundaries(str: String, pattern: Regex): IndexedSeq[Int] = {
    val boundaries = new mutable.BitSet
    for ( x <- (pattern findAllMatchIn str) ) {
      boundaries(x.start) = true
      boundaries(x.end) = true
    }
    boundaries(str.size) = true
    val bd = Array.fill(str.size + 1)(str.size)
    var i = 0
    for ( b <- boundaries ) {
      while ( i < b ) {
        bd(i) = b
        i += 1
      }
    }
    return bd
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
        val str = strings.mkString("\n")
        val bd = findBoundaries(str, new Regex(config.getString("boundary")))
        val es = new ExtremalSubstrings(SuffixArrays.build(str, config.getString("repeatsMethod")))
        val rps = es.maxRepeats(config.getInt("repeats").toInt, if (bd.size > 0 && bd(0) != str.size) {bd } else { (_ => str.size + 1) })
        for ( x <- rps ) {
          println(formatSpan(str, x))
        }
      }
      case "dfreq" => {
        val str = strings.mkString("\n")
        val bd = findBoundaries(str, new Regex(config.getString("boundary")))
        val sa = SuffixArrays.build(str, config.getString("repeatsMethod"))
        val es = new ExtremalSubstrings(sa)
        val rps = es.maxRepeats(config.getInt("repeats").toInt, if (bd.size > 0 && bd(0) != str.size) {bd } else { (_ => str.size + 1) })
        val cache = new mutable.HashMap[(Int,String),Set[Int]]
        val count = new mutable.HashMap[String,Set[Int]]
        val icount = new mutable.HashMap[(Int,String),Set[Int]]

        def tfind(s: String, d: Int) = sa.find(s).filter(bd(_) == d)
        def dfind(s: String) = sa.find(s).map(bd).toSet

        import org.whym.growthring.{NaiveExtremalSubstrings => NES}
        def tfindnaive(s: String, d: Int) = NES.find(str, s).filter(bd(_) == d)
        def dfindnaive(s: String) = NES.find(str, s).map(bd).toSet

        val ndocs = bd.toSet.size
        for ( pos <- rps ) {
          val doc = bd(pos._1)
          val slice = str.slice(pos._1, pos._2 + 1)
          for ( n <- Range.inclusive(config.getInt("minLen"), slice.size).reverse ) {
            for ( i <- Range.inclusive(0, slice.size - n) ) {
              val s = slice.slice(i, i+n)
              val atf = sa.find(s).size
              val tf = tfind(s, doc).size
              val df = dfind(s).size
              println("%d\t%d\t%d\t%d\t%d\t%d\t%.2f\t%d\t%s".format(pos._1, pos._1+i, pos._2+i+n, atf, tf, df, tf * scala.math.log(1.0 + 1.0*ndocs/df), s.size, s))
              // println("%s\t%s\t%d\t%d".format(tfind(s, doc).size == tfindnaive(s, doc).size,
              //                                 dfind(s).size == dfindnaive(s).size,
              //                                 tfindnaive(s, doc).size,
              //                                 dfindnaive(s).size
              //                               ))

              // val ls = cache.get((doc, s)) match {
              //   case Some(x) => x
              //   case None => {
              //     val v = sa.find(s)
              //     cache((doc, s)) = v
              //     v
              //   }
              // }
            }
          }
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
