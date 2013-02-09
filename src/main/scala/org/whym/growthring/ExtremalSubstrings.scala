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
   * Conversion from a string to an array of unsigned integers.  Needed by org.jsuffixarrays.
   */
  def stringToUnsigneds(str: String): Array[Int] = {
    val array = Array.fill(str.size * 2)(0)
    for ( (x,i) <- str.toCharArray.zipWithIndex ) {
      array(i * 2) = x & 0xFF
      array(i * 2 + 1) = x >>> 8
    }
    array
  }

  /**
   * Removal of overlapping spans (removing longer ones).  Remedy for artifacts caused by the char-unsigned conversion.
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
   * Removal of overlapping spans (removing shorter ones).  Remedy for artifacts caused by the char-unsigned conversion.
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
      ((x._1 - (x._1     & 1)) / 2,
       (x._2 + ((x._2+1) & 1)) / 2)

  def roundMin(x: (Int,Int)): (Int,Int) =
      ((x._1 + (x._1     & 1)) / 2,
       (x._2 - ((x._2+1) & 1)) / 2)

  def getHeight(text: Array[Int], pos: Array[Int]): Array[Int] = {
    val n = text.size
    val height = Array.fill(n)(0)
    val rank = Array.fill(n)(0)
    for ( i <- 0 until n ) {
      rank(pos(i)) = i
    }
    var h = 0
    for ( i <- 0 until n; if rank(i) > 0) {
      val j = pos(rank(i) - 1)
      while ( i + h < n && j + h < n && text(i + h) == text(j + h) ) {
        h += 1
      }
      height(rank(i)) = h
      if ( h > 0 ) {
        h -= 1
      }
    }
    height
  }
}

class ExtremalSubstrings(str: String, method: String = "jsuffixarrays") extends Logging {
  import ExtremalSubstrings._
  logger.info("start extremal substrings")
  private val arr = stringToUnsigneds(str)
  private val (sa, lcp_) = method match {
    case "sais" => {
      import com.sun.jna.{Library, Native, Memory, Pointer}
      trait SAIS extends Library {
        def sais(s: String, p: Pointer, n: Int): Int
        def sais_int(s: Pointer, p: Pointer, n: Int, k: Int): Int
      }
      logger.info("load sais")
      val sais = Native.loadLibrary("sais", classOf[SAIS]).asInstanceOf[SAIS]
      val mem1 = new Memory(str.size * 4 * 2) //TODO: str.size * 2 に節約
      val mem2 = new Memory(str.size * 4 * 2)
      mem1.write(0, arr, 0, arr.size)
      logger.info("start sais")
      sais.sais_int(mem1, mem2, arr.size, 256)
      val sa = Array.tabulate(arr.size)(i => mem2.getInt(i * 4))
      logger.info("start lcp")
      (sa, getHeight(arr, sa))
    }
    case _ => {
      import org.{jsuffixarrays => JSA}
      val builder = new JSA.DivSufSort()
      val sadata = JSA.SuffixArrays.createWithLCP(arr, 0, arr.size, builder)
      (sadata.getSuffixArray, sadata.getLCP)
      // val sa = sadata.getSuffixArray
      // (sa, getHeight(arr, sa))
    }
  }

  def minUniques(): Seq[(Int, Int)] = {
    val mu = Array.fill(arr.size + 1)(-1)
    val lcp = lcp_ ++ Array.fill(1)(-1)
    for ( i <- 0 until arr.size;
          l = lcp(i) max lcp(i+1)) {
      mu(sa(i) + l) = mu(sa(i) + l) max sa(i)
    } 
    return subsumeLonger(mu.zipWithIndex.filter(x => x._1 >= 0 && x._2 <= arr.size - 1).map(roundMax))
  }

  def minUniques2(): Seq[(Int, Int)] = {
    val mu = Array.fill(arr.size + 2)(-1)
    val lcp = lcp_ ++ Array.fill(2)(-1)
    //! 毎回 max min の組み合わせをやるよりも、補助配列をつくったほうがいい
    for ( (i,l) <- List((0, lcp(1) min lcp(2))) ++
                  (1 until arr.size).map{i => (i,
                                               (lcp(i-1) min lcp(i))
                                               max (lcp(i) min lcp(i+1))
                                               max (lcp(i+1) min lcp(i+2)))} ) {
      mu(sa(i) + l) = mu(sa(i) + l) max sa(i)
    } 
    return subsumeLonger(mu.zipWithIndex.filter(x => x._1 >= 0 && x._2 <= arr.size - 1).map(roundMax))
  }

  def maxRepeats(): Seq[(Int, Int)] = maxRepeats2

  def maxRepeats2(): Seq[(Int, Int)] = {
    val mr = Array.fill(arr.size + 1)(arr.size)
    val lcp = lcp_ ++ Array.fill(1)(-1)
    for ( i <- 0 until arr.size;
          l = lcp(i) max lcp(i+1); 
          if l >= 1 ) {
      mr(sa(i) + l - 1) = mr(sa(i) + l - 1) min sa(i)
    }
    return subsumeShorter(mr.zipWithIndex.filter(x => x._1 <= arr.size - 1).map(roundMin).filter(x => x._1 <= x._2))
  }

  def maxRepeats(n: Int =2): Seq[(Int, Int)] = {
    logger.info(s"start maxRepeats(${n})")
    val mr = Array.fill(arr.size + n + 1)(arr.size)
    val lcp = lcp_ ++ Array.fill(n)(-1)
    val lcpm_padded = Array.fill(n)(-1) ++ (0 until (lcp.size - n + 2)).map{i => lcp.slice(i, i+n-1).min}
    def lcpm(i: Int) = lcpm_padded(i + n)
    logger.info(s"start main loop")
    for ( i <- 0 until arr.size;
          l = ((i - n + 2) until (i + 2)).map(lcpm).max;
          if l >= 1 ) {
      mr(sa(i) + l - 1) = mr(sa(i) + l - 1) min sa(i)
    }
    logger.info(s"start main subsume")
    return subsumeShorter(mr.zipWithIndex.filter(x => x._1 <= arr.size - 1).map(roundMin).filter(x => x._1 <= x._2))
  }
}
