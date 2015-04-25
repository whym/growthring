/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.{mutable, immutable}
import com.typesafe.scalalogging.slf4j.Logging

/**
 * Maximal repeats and minimal unique substrings
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object ExtremalSubstrings {

  /**
   * Removal of overlapping spans (removing longer ones).  Remedy for artifacts caused by the char-uchar conversion.
   */
  def subsumeLonger(s: Seq[(Int,Int)]): Seq[(Int,Int)] =
    if (s.size == 0) {
      s
    } else {
      s.zip(Pair(-1,-1) +: s.slice(0, s.size-1)).filter{
        case ((x1,y1),(x2,y2)) => x1 > x2 && y1 > y2
      }.map(_._1)
    }

  /**
   * Removal of overlapping spans (removing shorter ones).  Remedy for artifacts caused by the char-uchar conversion.
   */
  def subsumeShorter(s: Seq[(Int,Int)]): Seq[(Int,Int)] =
    if (s.size == 0) {
      s
    } else {
      s.zip(s.tail :+ Pair(Int.MaxValue, Int.MaxValue)).filter{
        case ((x1,y1),(x2,y2)) => x1 < x2 && y1 < y2
      }.map(_._1)
    }

  def roundMax(x: (Int,Int)): (Int,Int) =
      ((x._1 - (x._1 & 1)) / 2,
        x._2 / 2)

  def roundMin(x: (Int,Int)): (Int,Int) =
      ((x._1 + (x._1     & 1)) / 2,
       (x._2 - ((x._2+1) & 1)) / 2)

  // def slidingMinimums(n: Int, arr: IndexedSeq[Int]): IndexedSeq[Int] = {
  //   (0 until (arr.size - n + 1)).map{i => arr.slice(i, i+n).min}
  // }

  def slidingMinimums(n: Int, arr: IndexedSeq[Int]): IndexedSeq[Int] = {
    class Count() {
      var p = 0
      override def toString = p.toString
    }
    import scala.language.reflectiveCalls
    var bag = new java.util.TreeMap[Int,Count] {
      def inc(x: Int) {
        if ( !this.containsKey(x) ) {
          this.put(x, new Count)
        }
        this.get(x).p += 1
      }
      def dec(x: Int) {
        if ( this.get(x) == null  ||  this.get(x).p == 1 ) {
          this.remove(x)
        } else {
          this.get(x).p -= 1
        }
      }
    }
    var seq = new mutable.Queue[Int]

    for ( x <- arr.slice(0, n).toList ) {
      bag.inc(x)
      seq.enqueue(x)
    }
    
    bag.firstKey +: (for ( x <- arr.slice(n, arr.size) ) yield {
      bag.inc(x)
      bag.dec(seq.head)
      seq.dequeue
      seq.enqueue(x)
      bag.firstKey
    }).toIndexedSeq
  }

}

class ExtremalSubstrings(array: SuffixArrays) extends Logging {
  private val arr = array.arr
  private val sa = array.sa
  private val lcp_ = array.lcp
  import ExtremalSubstrings._
  logger.info("start extremal substrings")

  private def gen_lcp_table(n: Int): Int=>Int = {
    val lcp = this.lcp_ ++ Array.fill(n)(-1)
    val lcpm_padded = Array.fill(n)(-1) ++ slidingMinimums(n - 1, lcp)
    def lcpm(i: Int) = lcpm_padded(i + n)
    return lcpm
  }

  def maxRepeats(n: Int =2, bd: Int=>Int = {_ => this.arr.size + 1}): Seq[(Int, Int)] = {
    logger.info(s"start maxRepeats(${n})")
    val lcpm = gen_lcp_table(n)
    val mr = Array.fill(this.arr.size + n + 1)(this.arr.size)
    logger.info(s"start main loop")
    for ( i <- 0 until this.arr.size;
          l = ((i - n + 2) until (i + 2)).map(lcpm).max;
          if l >= 1 ) {

      // bound by boundary positions; /2 and *2 are for converting
      // char positions and integer positions, see also roundMin and
      // roundMin
      val p = (this.sa(i) + l - 1) min (bd(this.sa(i)/2) * 2 - 1)
      // This is more flexible than post-processing to cut the emitted
      // spans from maxRepeats.  Consider a span with more than one
      // boundary in it, and it won't be trivial to efficiently split
      // it, especially when such spans overlap each other.

      mr(p) = mr(p) min this.sa(i)
    }
    logger.info(s"start subsume")
    return subsumeShorter(mr.zipWithIndex.filter(x => x._1 <= this.arr.size - 1).
      map(roundMin).filter(x => x._1 <= x._2))
  }

  def minUniques(n: Int =2, bd: Int=>Int = {_ => this.arr.size + 1}): Seq[(Int, Int)] = {
    logger.info(s"start minUniques(${n})")
    val lcpm = gen_lcp_table(n)
    val mu = Array.fill(this.arr.size + n + 1)(-1)
    logger.info(s"start main loop")
    for ( i <- 0 until this.arr.size;
          l = ((i - n + 2) until (i + 2)).map(lcpm).max ) {

      val p = (this.sa(i) + l) min (bd(this.sa(i)/2) * 2 - 1)
      mu(p) = mu(p) max this.sa(i)
    }
    logger.info(s"start subsume")
    return subsumeLonger(mu.zipWithIndex.filter(x => x._1 >= 0 && x._2 <= this.arr.size - 1).
      map(roundMax).filter(x => x._1 <= x._2))
  }

}
