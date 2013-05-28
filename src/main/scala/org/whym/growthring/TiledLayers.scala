/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
 * Given spans, pack them into smallest layers each of which has no overlaps
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object TiledLayers {

  def greedyTiling[T](body: Array[T], rp: Seq[(Int,Int)]): Seq[Set[(Int,Int)]] = {
    val starts = Array.fill(body.size)(-1)
    val ends = Array.fill(body.size)(-1)
    for ( (r,i) <- rp.zipWithIndex ) {
      starts(r._1) = i
    }
    val res = new mutable.HashSet[Int]
    for ( x <- Range(0, rp.size) ) {
      res.add(x)
    }
    var cur = new mutable.HashSet[Int]
    val ret = new mutable.ListBuffer[Set[Int]]
    while ( res.size > 0 ) {
      var n = 0
      while ( n < starts.size ) {
        val i = starts(n)
        if ( res(i) ) {
          cur += i
          res -= i
          n = rp(i)._2
        } else {
          n += 1
        }
      }
      ret.append(cur.toSet)
      cur.clear
    }
    return ret.map{s => s.map(rp)}
  }

}
