/**
 * DESCRIBE THIS PROGRAM HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

/**
 * DESCRIBE THIS CLASS HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object Main {

  def main(args: Array[String]) {
    import scala.io
    import scala.util.Properties
    val strings = (if (args.length > 0) {
      args.map(io.Source.fromFile(_).getLines.toList).flatMap(x => x).toList
    } else {
      io.Source.fromInputStream(System.in).getLines.toList
    })
    Properties.propOrElse("mode", "anonym") match {
      case "anonym" => {
        //! 改行はデフォルトで強制表示
        //! -Dunhide で指定したパターンは強制表示
        val str = strings.mkString("\n")
        val min_len = Properties.propOrElse("minLen", "2").toInt
        val es = new ExtremalSubstrings(str)
        val covered = for ( (s, e) <- es.maxRepeats(Properties.propOrElse("repeats", "2").toInt)
                           if e - s >= min_len ) yield (s, e)
        val flags = Properties.propOrElse("cover", "greedySliced") match {
          case "greedy" => Covering.greedy(str.toCharArray, covered)
          case _        => Covering.greedySliced(str.toCharArray, covered)
        }
        println(str.zip(Array.tabulate(str.length)(i => flags(i))).map(_ match {case (c,true) => c; case (c,false) => '_'}).mkString)
      }
      case "repeats" => {
        val str = strings.mkString("\n")
        val es = new ExtremalSubstrings(str)
        for ( x <- es.maxRepeats(Properties.propOrElse("repeats", "2").toInt) ) {
          println("r %d %d %s".format(x._1, x._2, new String(str.slice(x._1, x._2 + 1)))) //!
        }
        for ( x <- es.minUniques ) {
          println("u %d %d %s".format(x._1, x._2, new String(str.slice(x._1, x._2 + 1)))) //!
        }
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
  }
}
