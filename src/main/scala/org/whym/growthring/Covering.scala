/**
 * DESCRIBE THIS PROGRAM HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
 * DESCRIBE THIS CLASS HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object Covering {
  def any(rp: Seq[(Int,Int)]): Set[Int] =
    rp.map(x => (x._1 to x._2).toList).foldLeft(Set[Int]())(_++_).toSet

  def greedy(rp: Seq[(Int,Int)]): Set[Int] = {
    val max = (rp.map(_._2) ++ Seq(0)).max
    val flags = mutable.ArrayBuffer.fill(max + 2)(false)
    for ( (s,e) <- rp.sortBy(x => ((x._1 - x._2), x._1, x._2)) ) {
      if ( ( (s == 0) || flags(s-1) == false ) && flags(e+1) == false ) {
        for ( i <- s to e ) {
          flags(i) = true
        }
      }
    }
    return flags.zipWithIndex.filter(_._1).map(_._2).toSet
  }
}
