/**
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.{ mutable, Iterable }

/**
  * A set of functions to convert a set of repeats into text, annotated texts, etc
  *
  *  @author Yusuke Matsubara <whym@whym.org>
  */
object Repeat {
}

case class Repeat(length: Int, positions: Set[Int]) extends Iterable[(Int, Int)] {
  override def toString = {
    "Repeat(length=%s, positions=(%s))".format(length, positions.mkString(", "))
  }
  def spans = positions.map(i => (i, i + length))
  def iterator = spans.iterator
  def asSlicesOf[T](body: Seq[T]) = new Iterable[Seq[T]] {
    def iterator = spans.map{case (i, j) => body.slice(i, j)}.iterator
  }
}
