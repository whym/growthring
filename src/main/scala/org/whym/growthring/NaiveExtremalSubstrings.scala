/**
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

package org.whym.growthring

import scala.jdk.CollectionConverters._
import scala.collection.{ mutable, immutable }

/**
  * Maximal repeats and minimal unique substrings via enumerating all substrings
  *
  *  @author Yusuke Matsubara <whym@whym.org>
  */
object NaiveExtremalSubstrings {

  case class Substring(parent: String, start: Int, end: Int) {
    def length = end - start
    def head_removed = Substring(parent, start + 1, end)
    def last_removed = Substring(parent, start, end - 1)
    override def equals(x: Any) = x.isInstanceOf[Substring] && slice == x.asInstanceOf[Substring].slice
    def equalsAsString(x: Any) = slice == x
    private def slice = parent.substring(start, end)
    private val _hash = slice.hashCode
    override def hashCode = _hash
  }

  def substrings(str: String): Seq[Substring] =
    for (
      i <- 0 to str.length;
      j <- (i + 1) to str.length
    ) yield {
      Substring(str, i, j)
    }

  def count[T](seq: Seq[T]): Map[T, Seq[T]] = {
    val counts = new mutable.HashMap[T, List[T]] withDefault (_ => List())
    for (s <- seq) {
      counts(s) = s :: counts(s)
    }
    return counts.toMap
  }

  def countBounded[T](seq: Seq[T], bound: Int): Map[T, Seq[T]] = {
    val counts = new mutable.HashMap[T, List[T]] withDefault (_ => List())
    val overflown = new mutable.HashSet[T]
    for (s <- seq) {
      if (!(overflown contains s)) {
        counts(s) = s :: counts(s)
        if (counts(s).size > bound) {
          counts.remove(s)
          overflown += s
        }
      }
    }
    return counts.toMap
  }

  def find(body: String, q: String): Seq[Int] = substrings(body).filter(_.equalsAsString(q)).map(_.start)

  def minimals(set: Set[(Int, Int)]): Set[(Int, Int)] =
    set.filter(self => {
      !(set contains (self._1 + 1, self._2)) &&
        !(set contains (self._1, self._2 - 1))
    })

  def maximals(set: Set[(Int, Int)]): Set[(Int, Int)] =
    set.filter(self => {
      !(set contains (self._1 - 1, self._2)) &&
        !(set contains (self._1, self._2 + 1))
    })

  def minUniques(str: String, threshold: Int = 2) = {
    val counts = countBounded(substrings(str), threshold - 1)
    minimals(counts.values.flatten.map(x => (x.start, x.end - 1)).toSet).toList.sorted
  }

  def maxRepeats(str: String, threshold: Int = 2) = {
    val counts = count(substrings(str)).filter(x => x._2.size >= threshold)
    maximals(counts.values.flatten.map(x => (x.start, x.end - 1)).toSet).toList.sorted
  }

}
