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
  val lcp_ = sadata.getLCP

  def minUniques(): Seq[(Int, Int)] = {
    val mu = mutable.ArrayBuffer.fill(arr.size + 1)(-1)
    val lcp = lcp_ ++ Array.fill(1)(-1)
    for ( i <- 0 until arr.size;
          l = lcp(i) max lcp(i+1)) {
      mu(sa(i) + l) = mu(sa(i) + l) max sa(i)
    } 
    return mu.zipWithIndex.filter(x => x._1 >= 0 && x._2 <= arr.size - 1).map{
      x =>
        ((x._1 - (x._1 & 1)) / 2,
         (x._2 + (x._2 & 1)) / 2)
    }
  }

  def minUniques2(): Seq[(Int, Int)] = {
    val mu = mutable.ArrayBuffer.fill(arr.size + 2)(-1)
    val lcp = lcp_ ++ Array.fill(2)(-1)
    //! 毎回 max min の組み合わせをやるよりも、補助配列をつくったほうがいい
    for ( (i,l) <- List((0, lcp(1) min lcp(2))) ++
                  (1 until arr.size).map{i => (i,
                                               (lcp(i-1) min lcp(i))
                                               max (lcp(i) min lcp(i+1))
                                               max (lcp(i+1) min lcp(i+2)))} ) {
      mu(sa(i) + l) = mu(sa(i) + l) max sa(i)
    } 
    return mu.zipWithIndex.filter(x => x._1 >= 0 && x._2 <= arr.size - 1).map{
      x =>
        ((x._1 - (x._1 & 1)) / 2,
         (x._2 + (x._2 & 1)) / 2)
    }
  }

  def maxRepeats(): Seq[(Int, Int)] = maxRepeats(2)

  def maxRepeats2(): Seq[(Int, Int)] = {
    val mr = mutable.ArrayBuffer.fill(arr.size + 1)(arr.size)
    val lcp = lcp_ ++ Array.fill(1)(-1)
    for ( i <- 0 until arr.size;
          l = lcp(i) max lcp(i+1); 
          if l >= 1 ) {
      mr(sa(i) + l - 1) = mr(sa(i) + l - 1) min sa(i)
    }
    return mr.zipWithIndex.filter(x => x._1 <= arr.size - 1 && x._1 <= x._2 - 1).map{
      x =>
        ((x._1 + (x._1 & 1)) / 2,
         (x._2 - (x._2 & 1)) / 2)
    }
  }

  def maxRepeats3: Seq[(Int, Int)] = maxRepeats(3)

  def maxRepeats(n: Int =2): Seq[(Int, Int)] = {
    val mr = mutable.ArrayBuffer.fill(arr.size + n + 1)(arr.size)
    val lcp = lcp_ ++ Array.fill(n)(-1)
    val lcpm_padded = Array.fill(n)(-1) ++ (0 until (lcp.size - n + 2)).map{i => lcp.slice(i, i+n-1).min}
    def lcpm(i: Int) = lcpm_padded(i + n)
    for ( i <- 0 until arr.size;
          l = ((i - n + 2) until (i + 2)).map(lcpm).max;
          if l >= 1 ) {
      mr(sa(i) + l - 1) = mr(sa(i) + l - 1) min sa(i)
    }
    return mr.zipWithIndex.filter(x => x._1 <= arr.size - 1 && x._1 <= x._2 - 1).map{
      x =>
        ((x._1 + (x._1 & 1)) / 2,
         (x._2 - (x._2 & 1)) / 2)
    }
  }
}
