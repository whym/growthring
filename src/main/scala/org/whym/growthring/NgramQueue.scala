/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.mutable

class NgramQueue[T](val minSize: Int, val maxSize: Int) {
  private var n = 0
  private val queues = (minSize to maxSize).reverse.map(n => new SlidingQueue[T](n))

  def getNgrams() =
    queues.filter(q => q.size == q.maxSize).map(_.toList) // return queues that are filled

  def clear() = {
    n = 0
    queues.foreach(q => q.clear)
  }
  def enqueue(token: T) = {
    n += 1
    queues.foreach(q => q.enqueue(token))
  }
}

class SlidingQueue[A](val maxSize : Int) extends mutable.Queue[A] {
  override def enqueue(elems: A*) = {
    elems.foreach(super.enqueue(_))
    while (this.size > this.maxSize) {
      this.dequeue()
    }
  }
}
