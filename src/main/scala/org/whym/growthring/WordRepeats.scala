/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.matching.Regex
import org.whym.growthring.{NaiveExtremalSubstrings => NES}

class WordRepeats(sep: Regex ="[ \u3000　\n\r	\t]+".r) {
  def repeats(str: String, k: Int, minLen: Int=1) = {
    val counts = new mutable.HashMap[String, mutable.Set[(Int,Int)]] with mutable.MultiMap[String, (Int,Int)]
    for ( s <- (Pair(0,0) +: (sep findAllMatchIn str).map(x => (x.start,x.end)).toList :+ Pair(str.length, str.length)).sliding(2) ) {
      val start = s(0)._2
      val end   = s(1)._1 - 1
      counts.addBinding(str.slice(start,end+1), (start,end))
    }
    counts.filter(_._2.size >= k).map(_._2).reduce(_++_).toList
  }
}
