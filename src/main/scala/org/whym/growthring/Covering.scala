/**
 * DESCRIBE THIS PROGRAM HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
 * Given spans, fill the parent sequence with some of the spans so that no overlap nor concatenation happens
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object Covering {
  def any[T](body: IndexedSeq[T], rp: Seq[(Int,Int)]): Set[Int] = // Note: this violates the definition above by possibly concatenating two or more spans
    rp.map(x => (x._1 to x._2).toList).foldLeft(Set[Int]())(_++_).toSet

  def greedy[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] = {
    val max = (rp.map(_._2) ++ Seq(0)).max
    val flags = mutable.ArrayBuffer.fill(max + 2)(false)
    for ( (s,e) <- rp.sortBy(x => ((x._1 - x._2), x._1, x._2)) ) {
      if ( ( (s == 0) || flags(s-1) == false ) && flags(e+1) == false ) {
        for ( i <- s to e ) {
          flags(i) = true
        }
      }
    }
    return flags.zipWithIndex.filter(_._1).map(_._2).toSet
  }

  def greedyConservative[T](body: IndexedSeq[T], rp: Seq[(Int,Int)]): Set[Int] = {
    val max = (rp.map(_._2) ++ Seq(0)).max
    val flags = mutable.ArrayBuffer.fill(max + 2)(false)
    val invalidated = new mutable.HashMap[IndexedSeq[T], Int]{ override def default(x:IndexedSeq[T]) = 0 }
    val groups = rp.groupBy(x => body.slice(x._1, x._2+1).toIndexedSeq)
    val min_freq = groups.map(_._2.size).min
    for ( (seg, ls) <- groups.toList.sortBy(x => (- x._1.size, x._2.size)) ) {
      val checks = for ( (s,e) <- ls ) yield ( ( (s == 0) || flags(s-1) == false ) && flags(e+1) == false )
      if ( checks.reduce(_ && _)  &&  ls.size - invalidated(seg) >= min_freq ) {
        for ( (s,e) <- ls; i <- s to e ) {
          flags(i) = true
        }
        for ( i <- 0 until seg.size; j <- i + 1 until seg.size ) {
          invalidated(seg.slice(i,j)) += 1
        }
      }
    }
    return flags.zipWithIndex.filter(_._1).map(_._2).toSet
  }

  def greedySliced[T](body: IndexedSeq[T], rp: Seq[(Int,Int)]): Set[Int] = {
    import scala.collection.mutable.PriorityQueue
    val max = (rp.map(_._2) ++ Seq(0)).max
    val flags = mutable.ArrayBuffer.fill(max + 2)(false)
    val queue = new PriorityQueue[(Int,Int)]()(Ordering.by[(Int,Int),Int](x => x._2 - x._1))
    for ( x <- rp ) {
      queue.enqueue(x)
    }

    def closestAvailable(span: (Int,Int)): (Int,Int) = {
      val start = (span._1 to span._2).find(i => (i > 0 && flags(i - 1) == false) || (i == 0 && flags(0) == false))
      val end = (span._1 to span._2).reverse.find(i => flags(i + 1) == false)
      (start match { case Some(x) => x; case _ => span._1 },
       end   match { case Some(x) => x; case _ => span._2 })
    }
    
    while ( queue.size > 0 ) {
      val h = queue.dequeue
      val (s,e) = h
      if ( ( (s == 0) || flags(s-1) == false ) && flags(e+1) == false ) {
        for ( i <- s to e ) {
          flags(i) = true
        }
      } else {
        // slice and enqueue
        val n = closestAvailable((s,e))
        if ( n != (s,e) && n._2 >= n._1 ) {
          queue.enqueue(n)
        }
      }
    }
    return flags.zipWithIndex.filter(_._1).map(_._2).toSet
  }

  // observe that the covering problem can be casted into a max-weight matching problem by translating spans into vertices.
  // vertex weights are given by span length, edge weights are given by max of the two end-points.
  // use algorithms to find optimals (slow), or approximate
  //def optimal(rp: Seq[(Int,Int)]): Set[Int]
  //def nearOptimal(rp: Seq[(Int,Int)]): Set[Int]
}
