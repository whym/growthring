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
 * Suffix array utilities
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object SuffixArrays extends Logging {
  /**
   * Conversion from a string to an array of unsigned chars (in int array).  Needed by org.jsuffixarrays.
   */
  def stringToUchars(str: String): Array[Int] = {
    val array = Array.fill(str.size * 2)(0)
    for ( (x,i) <- str.toCharArray.zipWithIndex ) {
      array(i * 2) = x & 0xFF
      array(i * 2 + 1) = x >>> 8
    }
    array
  }

  /**
   * Calculating a longest common prefix array from a suffix array.
   * Adapted from Linear-Time Longest-Common-Prefix Computation in Suffix Arrays and Its Applications (Toru Kasai, Gunho Lee, Hiroki Arimura, Setsuo Arikawa and Kunsoo Park, 2009)
   */
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

  def build(str: String, method: String = "jsuffixarrays"): SuffixArrays = {
    logger.info("start suffixarrays build")
    val arr = stringToUchars(str)
    method match {
      case "sais" => {
        // TODO: 関数にして単体テスト
        import com.sun.jna.{Library, Native, Memory, Pointer}
          trait SAIS extends Library {
            def sais(s: Pointer, p: Pointer, n: Int): Int
            //def sais_int(s: Pointer, p: Pointer, n: Int, k: Int): Int
          }
        logger.info("load sais")
        val sais = Native.loadLibrary("sais", classOf[SAIS]).asInstanceOf[SAIS]
        val mem1 = new Memory(str.size * 2)
        val mem2 = new Memory(str.size * 4 * 2)
        mem1.write(0, arr.map(_.asInstanceOf[Byte]), 0, arr.size)
        logger.info("start sais")
        sais.sais(mem1, mem2, arr.size)
        //sais.sais_int(mem1, mem2, arr.size, 256)
        val sa = Array.tabulate(arr.size)(i => mem2.getInt(i * 4))
        logger.info("start lcp")
        SuffixArrays(arr, sa, getHeight(arr, sa))
      }
      case _ => {
        import org.{jsuffixarrays => JSA}
        val builder = new JSA.DivSufSort()
        val sadata = JSA.SuffixArrays.createWithLCP(arr, 0, arr.size, builder)
        SuffixArrays(arr, sadata.getSuffixArray, sadata.getLCP)
        // val sa = sadata.getSuffixArray
        // (sa, getHeight(arr, sa))
      }
    }
  }
}

case class SuffixArrays(arr: Array[Int], sa: Array[Int], lcp: Array[Int])
