/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.{mutable, immutable}
import com.typesafe.scalalogging.slf4j.Logging
import java.{io => jio}
import java.nio

/**
 * Suffix array utilities
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object SuffixArrays extends Logging {

  val HEAD = "GAR1".getBytes
  val INTSIZE = 4

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

  def buildSais(str: String): SuffixArrays = {
    logger.info("start sais build")
    val arr = stringToUchars(str)
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

  def buildJsuffixarrays(str: String): SuffixArrays = {
    logger.info("start jsuffixarrays build")
    val arr = stringToUchars(str)
    import org.{jsuffixarrays => JSA}
    val builder = new JSA.DivSufSort()
    val sadata = JSA.SuffixArrays.createWithLCP(arr, 0, arr.size, builder)
    SuffixArrays(arr, sadata.getSuffixArray, sadata.getLCP)
  }

  def build(str: String, method: String = "jsuffixarrays"): SuffixArrays = {
    method match {
      case "sais" => buildSais(str)
      case _ =>      buildJsuffixarrays(str)
    }
  }

  implicit class RichByteBuffer(val b: nio.ByteBuffer) extends AnyVal {
    def getBytes(n: Int) =   { val a = new Array[Byte](n); b.get(a); a }
    def getShorts(n: Int) =  { val a = new Array[Short](n);  var i=0; while (i<n) { a(i)=b.getShort();  i+=1 } ; a }
    def getInts(n: Int) =    { val a = new Array[Int](n);    var i=0; while (i<n) { a(i)=b.getInt();    i+=1 } ; a }
    def getLongs(n: Int) =   { val a = new Array[Long](n);   var i=0; while (i<n) { a(i)=b.getLong();   i+=1 } ; a }
    def getFloats(n: Int) =  { val a = new Array[Float](n);  var i=0; while (i<n) { a(i)=b.getFloat();  i+=1 } ; a }
    def getDoubles(n: Int) = { val a = new Array[Double](n); var i=0; while (i<n) { a(i)=b.getDouble(); i+=1 } ; a }
  }

  def store(a: SuffixArrays, out: jio.FileOutputStream): Option[Int] = store(a.arr, a.sa, out)

  def store(array: IndexedSeq[Int], sa: IndexedSeq[Int], out: jio.FileOutputStream): Option[Int] = {
    val fc = out.getChannel
    val header = nio.ByteBuffer.allocate(HEAD.length + INTSIZE)
    var size = 0
    header.put(HEAD)
    header.putInt(array.length)
    header.flip
    size += fc.write(header)

    val ints = nio.ByteBuffer.allocate(INTSIZE * sa.length)
    for ( x <- sa ) {
      ints.putInt(x)
    }
    ints.flip
    size += fc.write(ints)

    val chars = nio.ByteBuffer.allocate(array.length)
    for ( x <- array ) {
      chars.put((x-128).asInstanceOf[Byte])
    }
    chars.flip
    size += fc.write(chars)

    out.flush
    return Some(size)
  }

  def load(in: jio.FileInputStream): Option[SuffixArrays] = {
    val fc = in.getChannel
    val header = nio.ByteBuffer.allocate(HEAD.length + INTSIZE)
    println("fc read " + fc.read(header))
    val hbytes = new Array[Byte](HEAD.length)
    header.clear
    header.get(hbytes)
    if ( new String(hbytes) != new String(HEAD) ) {
      println("wrong header: " + hbytes)
      return None
    }
    val nrec = header.getInt

    val ints = nio.ByteBuffer.allocate(INTSIZE * nrec)
    fc.read(ints)
    ints.clear

    val chars = nio.ByteBuffer.allocate(nrec)
    fc.read(chars)
    chars.clear

    val arr = chars.getBytes(nrec).map(_.asInstanceOf[Int] + 128)
    val sa = ints.getInts(nrec)
    Some(SuffixArrays(arr,
                      sa,
                      getHeight(arr, sa)))
  }

}

case class SuffixArrays(arr: IndexedSeq[Int], sa: IndexedSeq[Int], lcp: IndexedSeq[Int]) {
  def find(q: IndexedSeq[Int]): Seq[Int] = {
    val l = _lowerbound(q)
    val r = _upperbound(q)
    return Range(l,r).map(sa(_) / 2)
  }
  private def _lowerbound(q: IndexedSeq[Int]): Int = {
    def cmp(i: Int): Int = {
      for ( j <- Range(0, q.size) ) {
        if (sa(i) + j >= arr.size) {
          return -1
        }
        if (q(j) != arr(sa(i) + j)) {
          return arr(sa(i) + j) - q(j)
        }
      }
      return 0
    }
    var l = -1
    var r = arr.size
    while ( true ) {
      var m = l + (r - l) / 2
      if ( m <= l ) {
        return r
      }
      if (cmp(m) < 0) {
        l = m
      } else {
        r = m
      }
    }
    return r
  }
  private def _upperbound(q: IndexedSeq[Int]): Int = {
    def cmp(i: Int): Int = {
      for ( j <- Range(0, q.size) ) {
        if (sa(i) + j >= arr.size) {
          return +1
        }
        if (q(j) != arr(sa(i) + j)) {
          return arr(sa(i) + j) - q(j)
        }
      }
      return 0
    }
    var l = -1
    var r = arr.size
    while ( true ) {
      var m = l + (r - l) / 2
      if ( m <= l ) {
        return r
      }
      if (cmp(m) <= 0) {
        l = m
      } else {
        r = m
      }
    }
    return l+1
  }
}

