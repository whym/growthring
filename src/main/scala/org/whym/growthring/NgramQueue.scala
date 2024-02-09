/**
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */

package org.whym.growthring

import scala.collection.mutable

class NgramQueue[T](val minSize: Int, val maxSize: Int) {
  private var n = 0
  private val queues =
    (minSize to maxSize).reverse.map(n => new SlidingQueue[T](n))

  def getNgrams() =
    queues.filter(q => q.size == q.maxSize).map(
      _.toList
    ) // return queues that are filled

  def clear() = {
    n = 0
    queues.foreach(q => q.clear())
  }
  def enqueue(token: T) = {
    n += 1
    queues.foreach(q => q.enqueue(token))
  }
}

class SlidingQueue[A](val maxSize: Int) extends mutable.Queue[A] {
  override def enqueue(e: A) = {
    super.enqueue(e)
    if this.size > this.maxSize then {
      this.dequeue()
    }
    this
  }

  override def enqueue(e1: A, e2: A, elems: A*) = {
    super.enqueue(e1)
    super.enqueue(e2)
    super.enqueueAll(elems)
    while this.size > this.maxSize do {
      this.dequeue()
    }
    this
  }
}

// TODO: オブジェクトの生成の頻度を抑える（lazy にする）。ハッシュ値を rotate hash の方法で求め、記憶しておく。
