/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.{mutable, immutable}

/**
 * Maximal repeats and minimal unique substrings via enumerating all substrings
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object NaiveExtremalSubstrings {

  case class Substring(parent: String, start: Int, end: Int, slice: String) {
    def length = end - start
    def head_removed  = Substring(parent, start+1, end, parent.slice(start+1, end))
    def last_removed  = Substring(parent, start, end-1, parent.slice(start, end-1))
    override def equals(x: Any) = x.isInstanceOf[Substring] && slice == x.asInstanceOf[Substring].slice
    override def hashCode = slice.hashCode
  }

  def substrings(str: String): Seq[Substring] =
    for ( i <- 0 to str.length;
          j <- (i+1) to str.length ) yield {
            Substring(str, i, j, str.slice(i,j))
         }

  def count[T](seq: Seq[T]): Map[T,List[T]] = {
    val counts = new mutable.HashMap[T,List[T]] {
      override def default(x:T) = List[T]()
    }
    for ( s <- seq ) {
      counts(s) = s :: counts(s)
    }
    return counts.toMap
  }

  // def minimals(set: Set[Substring]): Set[Substring] =
  //   set.filter(str => {
  //     ( str.length <= 1 || !(set contains str.head_removed) ) &&
  //     ( str.length <= 1 || !(set contains str.last_removed) )
  //   })

  def minimals(set: Set[(Int,Int)]): Set[(Int,Int)] =
    set.filter(self => {
      !(set contains Pair(self._1+1, self._2)) &&
      !(set contains Pair(self._1,   self._2-1))
    })

  def maximals(set: Set[(Int,Int)]): Set[(Int,Int)] =
    set.filter(self => {
      !(set contains Pair(self._1-1, self._2)) &&
      !(set contains Pair(self._1,   self._2+1))
    })

  def minUniques(str: String, threshold: Int=1) = {
    val counts = count(substrings(str)).filter(x => x._2.size <= threshold)
    //minimals(counts.keySet).map(counts).reduce(_++_).map(x => (x.start, x.end - 1)).toList.sorted
    minimals(counts.values.reduce(_++_).map(x => (x.start, x.end - 1)).toSet).toList.sorted
  }

  def maxRepeats(str: String, threshold: Int=2) = {
    val counts = count(substrings(str)).filter(x => x._2.size >= threshold)
    maximals(counts.values.reduce(_++_).map(x => (x.start, x.end - 1)).toSet).toList.sorted
  }

}
