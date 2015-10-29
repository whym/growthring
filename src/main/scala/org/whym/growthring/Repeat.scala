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

case class Repeat[T](body: Seq[T], length: Int, positions: Set[Int]) extends Iterable[Seq[T]] {
  def iterator = positions.map(i => {
    val x: Seq[T] = body.slice(i, i + length)
    x
  }).iterator
  override def toString = {
    "Repeat(body=(%s), length=%s, positions=(%s))".format(body.mkString(", "), length, positions.mkString(", "))
  }
}
