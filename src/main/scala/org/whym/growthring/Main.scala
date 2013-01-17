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
    val strings = args.map(io.Source.fromFile(_).getLines.toList).flatMap(x => x).toList
    Properties.propOrElse("mode", "repeats") match {
      case "repeats" => {
        val es = new ExtremalSubstrings(strings.mkString("\n"))
        for ( x <- es.maxRepeats(Properties.propOrElse("maxRepeats", "3").toInt) ) {
          println("r " + new String(strings.mkString("\n").slice(x._1, x._2 + 1))) //!
        }
          for ( x <- es.minUniques ) {
            println("u " + new String(strings.mkString("\n").slice(x._1, x._2 + 1))) //!
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
