/**
 * DESCRIBE THIS PROGRAM HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.{mutable, immutable}

/**
 * DESCRIBE THIS CLASS HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object ExtremalSubstrings {
  def stringToUnsigneds(str: String): Array[Int] =
    str.toCharArray.map(x => List((x & 0xFF),
                                  (x >>> 8))).reduce((s,x) => s ++ x).toArray
}

class ExtremalSubstrings(str: String) {
  import ExtremalSubstrings._

  import org.{jsuffixarrays => JSA}
  val dsf = new JSA.DivSufSort()
  val arr = stringToUnsigneds(str)
  val sadata = JSA.SuffixArrays.createWithLCP(arr, 0, arr.size, dsf)
  val sa  = sadata.getSuffixArray
  val lcp = sadata.getLCP ++ Array(0)

  def minUniques: Seq[(Int, Int)] = {
    val mu = mutable.ArrayBuffer.fill(arr.size + 1)(-1)
    for ( i <- 0 until arr.size ) {
      val l = lcp(i) max lcp(i + 1)
      mu(sa(i) + l) = mu(sa(i) + l) max sa(i)
    } 
    return mu.zipWithIndex.filter(x => x._1 >= 0 && x._2 <= arr.size - 1).map{
      x =>
        ((x._1 - (x._1 & 1)) / 2,
         (x._2 + (x._2 & 1)) / 2)
    }
  }

  def maxRepeats: Seq[(Int, Int)] = {
    val mr = mutable.ArrayBuffer.fill(arr.size)(arr.size)
    for ( i <- 0 until arr.size ) {
      val l = lcp(i) max lcp(i + 1)
      if ( sa(i) + l >= 1 ) {
        mr(sa(i) + l - 1) = mr(sa(i) + l - 1) min sa(i)
      }
    }
    return mr.zipWithIndex.filter(x => x._1 <= arr.size - 1 && x._1 <= x._2).map{
      x =>
        ((x._1 + (x._1 & 1)) / 2,
         (x._2 - (x._2 & 1)) / 2)
    }.filter(x => x._1 <= x._2)         // necessary because the rounding above sometimes creates this flipped interval
  }
}
