/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.mutable

class NgramRepeats(val n: Int) {
  def repeats(str: String, k: Int, minLen: Int=1) = {
    val ng = new NgramQueue[Character](minLen, n)
    val pos  = new mutable.HashMap[Seq[Character], mutable.Set[Int]] with mutable.MultiMap[Seq[Character], Int]
    for ( (s,i) <- str.toList.zipWithIndex ) yield {
      ng enqueue s
      for ( x <- ng.getNgrams.map(_.toList) ) {
        pos.addBinding(x, i)
      }
    }
    pos.filter(_._2.size >= k).keys.map(x => pos(x).map(y => (y - x.length + 1, y)).toSet).reduce(_++_).toList
  }
}
