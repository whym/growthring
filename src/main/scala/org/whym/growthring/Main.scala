/**
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

package org.whym.growthring
import scala.jdk.CollectionConverters._
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable
import scala.util.matching.Regex

case class ExecMode(help: Seq[String], exec: Seq[String] => Unit)

/**
  * Main entry point
  *
  *  @author Yusuke Matsubara <whym@whym.org>
  */
object Main extends LazyLogging {

  def findCovereds(long: Seq[(Int, Int)], short: Seq[(Int, Int)], supports: Int): Seq[((Int, Int), Seq[(Int, Int)])] = {
    def inside(x: (Int, Int), y: (Int, Int)) = (x._1 >= y._1 && x._2 <= y._2)
    val ret = new mutable.ArrayBuffer[((Int, Int), Seq[(Int, Int)])]
    val lit = long.iterator.buffered
    while (lit.hasNext) {
      val sit = short.iterator.buffered
      while (sit.hasNext && sit.head._1 < lit.head._1) {
        sit.next()
      }
      val buf = new mutable.ArrayBuffer[(Int, Int)]
      while (sit.hasNext && inside(sit.head, lit.head)) {
        buf.append(sit.head)
        sit.next()
      }
      if (buf.size >= supports) {
        ret.append((lit.head, buf.toSeq))
      }
      lit.next()
    }
    return ret.toSeq
  }

  def anonymize(
    rmethod:        (String, Int) => Seq[(Int, Int)],
    cmethod:        (Array[Char], Seq[(Int, Int)], Int) => Set[Int],
    strings:        Seq[String],
    min_len:        Int,
    cover_char:     Char,
    freq:           Int,
    unhide_pattern: String                                          = "",
    gap:            Int,
    _start:         Int,
    _end:           Int): Seq[String] = {

    val str = strings.mkString("\n")
    logger.debug(f"${str.length}%d characters, frequency at least ${freq}%d, each unprotected span at least ${min_len}%d in length.")
    val covered = for ((s, e) <- rmethod(str, freq); if e - s + 1 >= min_len) yield (s, e)

    val start = if (_start >= 0) { _start } else { 0 }
    val end = if (_end > 0) { _end } else { str.length }
    val coveredFiltered = covered.filter(x => (start <= x._1 && x._1 < end) || (start <= x._2 && x._2 < end))
    logger.debug(f"${coveredFiltered.size}%d / ${covered.size}%d repeats from ${start}%d to ${end}%d.")
    val flags = cmethod(str.toCharArray, coveredFiltered, gap)
    logger.debug(f"${flags.size} characters unsuppressed.")
    val unhides = new mutable.BitSet
    for (x <- (new Regex(unhide_pattern) findAllMatchIn str)) {
      for (i <- Range(x.start, x.end)) {
        unhides(i) = true
      }
    }
    logger.debug(f"${unhides.size} matched to regex ${unhide_pattern}.")
    val mflags = flags ++ unhides
    str.zip(Array.tabulate(str.length)(i => mflags(i))).map { case (c, true) => c; case (c, false) => cover_char }.mkString.slice(start, end).split('\n').toSeq
  }

  def formatSpan(str: String, x: (Int, Int)): String = {
    import org.apache.commons.text.{ StringEscapeUtils => SEU }
    val f = str.slice(x._1, x._2 + 1).replace("\n", " ")
    s"${x._1}\t${x._2}\t${SEU.escapeJava(new String(str.slice(x._1, x._2 + 1)))}\t${f}"
  }

  def findBoundariesAsArray(str: String, pattern: Regex): IndexedSeq[Int] = {
    val boundaries = new mutable.BitSet
    for (x <- (pattern findAllMatchIn str)) {
      boundaries(x.start) = true
      boundaries(x.end) = true
    }
    boundaries(str.length) = true
    val bd = Array.fill(str.length + 1)(str.length)
    var i = 0
    for (b <- boundaries) {
      while (i < b) {
        bd(i) = b
        i += 1
      }
    }
    return scala.collection.immutable.ArraySeq.unsafeWrapArray(bd)
  }

  def findBoundaries(str: String, pattern: Regex): Int => Int = {
    val bd = findBoundariesAsArray(str, pattern)
    return if (bd.length > 0 && bd(0) != str.length) { bd } else { _ => str.length + 1 }
  }

  def main(args: Array[String]): Unit = {
    import com.typesafe.config.ConfigFactory
    val config = ConfigFactory.load.getConfig("org.whym.growthring")

    logger.info("**** main begin ****")
    logger.info("build info = " + buildInfo)
    logger.info("config = " + config.entrySet.asScala.mkString("\t"))
    scala.sys.addShutdownHook {
      logger.info("**** shutdown ****")
    }

    val modes: Map[String, ExecMode] = Map(
      "multiple-anonym" -> ExecMode(Seq("(not implemented)"), { strings =>
        import scala.xml.parsing.XhtmlParser
        logger.debug("multiple-anonym is not implemented")
        for (config <- XhtmlParser(io.Source.fromFile(config.getString("configFile"))) \\ "config") {
          println((config \ "file").text)
        }
      }),

      "anonym" -> ExecMode(Seq("k-anonymize"), { strings =>
        val rmethodstr = config.getString("repeatsMethod")
        val cmethodstr = config.getString("coveringMethod")
        logger.debug(f"repeats type '${rmethodstr}'")
        logger.debug(f"covering type '${cmethodstr}'")
        val rmethod: (String, Int) => Seq[(Int, Int)] = config.getString("repeatsMethod") match {
          case "naive" => NaiveExtremalSubstrings.maxRepeats
          case "word"  => new WordRepeats().repeats
          case "ngram" => new NgramRepeats(config.getInt("ngramSize")).repeats(_, _, config.getInt("minLen").toInt)
          case m       => { (s: String, r: Int) => new ExtremalSubstrings(SuffixArrays.build(s, m)).maxRepeats(r) }
        }

        val cmethod: (Array[Char], Seq[(Int, Int)], Int) => Set[Int] = config.getString("coveringMethod") match {
          case "greedyLength" => Covering.greedyLength
          //case "greedyConservative" => Covering.greedyConservative
          case "greedySliced" => Covering.greedySliced
          case "exhaustive"   => Covering.exhaustive
          case "dp"           => Covering.dp
          case x => {
            logger.debug(s"covering method '${x}' not found; using default covering algorithm")
            Covering.greedyLengthFreq
          }
        }
        for (
          s <- anonymize(
            rmethod,
            cmethod,
            strings,
            config.getInt("minLen").toInt,
            config.getString("coverChar")(0),
            config.getInt("repeats").toInt,
            config.getString("unhide"),
            config.getInt("gap"),
            config.getInt("start"),
            config.getInt("end"))
        ) {

          println(s)
        }
      }),

      "msa" -> ExecMode(Seq("find multiple sequence alignment"), { strings =>
        val msa = new MultipleSequenceAlignment[Char](strings.map(x => ("^" + x + "$").toCharArray.toIndexedSeq))
        val dag = msa.align().compact((x, y) => x.concat(y))
        def nodeformat(i: Int, x: MultipleSequenceAlignment.Node[Char]): String = {
          def escape(x: String) = x.replace("\\", "\\\\").replace("\"", "\\\"")
          "  N_%d[label=\"%s\",fontsize=%f];".format(i, escape(x.label.map(_.toString).mkString),
            10 + scala.math.log(x.freq) * 4)
        }
        for (line <- dag.dot(nodeformat)) {
          println(line)
        }
      }),

      "dfreq" -> ExecMode(Seq("calculate document frequencies"), { strings =>
        val str = strings.mkString("\n")
        val bd = findBoundariesAsArray(str, new Regex(config.getString("boundary")))
        val sa = SuffixArrays.build(str, config.getString("repeatsMethod"))
        val es = new ExtremalSubstrings(sa)
        val rps = es.maxRepeats(config.getInt("repeats").toInt, bd)
        val cache = new mutable.HashMap[(Int, String), Set[Int]]
        val count = new mutable.HashMap[String, Set[Int]]
        val icount = new mutable.HashMap[(Int, String), Set[Int]]

        def tfind(s: String, d: Int) = sa.find(s).filter(bd(_) == d)
        def dfind(s: String) = sa.find(s).map(bd).toSet

        import org.whym.growthring.{ NaiveExtremalSubstrings => NES }
        def tfindnaive(s: String, d: Int) = NES.find(str, s).filter(bd(_) == d)
        def dfindnaive(s: String) = NES.find(str, s).map(bd).toSet

        val ndocs = bd.toSet.size
        for (pos <- rps) {
          val doc = bd(pos._1)
          val slice = str.slice(pos._1, pos._2 + 1)
          for (n <- Range.inclusive(config.getInt("minLen"), slice.length).reverse) {
            for (i <- Range.inclusive(0, slice.length - n)) {
              val s = slice.slice(i, i + n)
              val atf = sa.find(s).size
              val tf = tfind(s, doc).size
              val df = dfind(s).size
              println("%d\t%d\t%d\t%d\t%d\t%d\t%.2f\t%d\t%s".format(pos._1, pos._1 + i, pos._2 + i + n, atf, tf, df, tf * scala.math.log(1.0 + 1.0 * ndocs / df), s.size, s))
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
      }),

      "nested" -> ExecMode(Seq(
        "Find nested maximal substrings.",
        "As parameters, repeats, nested.repeats and nested.supports are required.",
        "See nested.conf."), { strings =>
        val str = strings.mkString("\n")
        val bd = findBoundaries(str, new Regex(config.getString("boundary")))
        val es = new ExtremalSubstrings(SuffixArrays.build(str, config.getString("repeatsMethod")))
        val mr = config.getInt("repeats").toInt
        val mu = config.getInt("nested.repeats").toInt
        val supports = config.getInt("nested.supports").toInt
        if (mr > mu) {
          logger.debug("warning: repeats (%s) should not be greater than nested repeats (%s)".format(mr, mu))
        }
        val rps = es.maxRepeats(mr, bd)
        val uqs = if (config.getBoolean("nested.useUniques")) {
          es.minUniques(mu, bd)
        } else {
          es.maxRepeats(mu, bd)
        }

        for ((rp, uq) <- findCovereds(rps, uqs, supports)) {
          println("r\t" + formatSpan(str, rp))
          for (u <- uq) {
            println("  u\t" + formatSpan(str, u))
          }
        }
      }),

      "repeats" -> ExecMode(Seq("find repeats"), { strings =>
        val str = strings.mkString("\n")
        val bd = findBoundaries(str, new Regex(config.getString("boundary")))
        val es = new ExtremalSubstrings(SuffixArrays.build(str, config.getString("repeatsMethod")))
        val rps = es.maxRepeats(config.getInt("repeats").toInt, bd)
        val uqs = es.minUniques(config.getInt("uniques").toInt, bd)
        for (x <- rps) {
          println("r\t" + formatSpan(str, x))
        }
        for (x <- uqs) {
          println("u\t" + formatSpan(str, x))
        }
      }))

    val modeName = config.getString("mode") match {
      // map aliases
      case "segment" => "repeats"
      case x         => x
    }
    modes.get(modeName) match {
      case Some(exec) =>
        import scala.io
        val strings = if (args.length > 0) {
          args.flatMap(io.Source.fromFile(_).getLines().toList).toList
        } else {
          io.Source.fromInputStream(System.in).getLines().toList
        }
        logger.debug(f"${strings.size}%d lines.")
        exec.exec(strings)
      case None =>
        System.err.println(buildInfo)
        System.err.println(s"mode '${modeName}' not found; choose one from ${modes.keys.mkString(", ")} and set it to -Dorg.whym.growthring.mode=:")
        modes.foreach {
          case (name, e) => {
            val help = e.help.map { "    " + _ }.mkString("\n")
            System.err.println(s"org.whym.growthring.mode=$name")
            System.err.println(help)
          }
        }
    }

    logger.info("**** main end   ****")
  }

  def buildInfo: String = BuildInfo.toMap.mkString(", ")
}
