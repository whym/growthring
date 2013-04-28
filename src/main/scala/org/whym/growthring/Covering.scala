/**
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

  def greedyLength[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] = {
    val sorted = new mutable.HashMap[Int,mutable.Set[(Int,Int)]] with mutable.MultiMap[Int, (Int,Int)]
     for ( ent <- rp ) {
      sorted.addBinding(ent._1 - ent._2, ent)
     }
     val flags = Array.fill(body.size + 2)(false)
     if ( sorted.size == 0 ) {
       return Set()
     }
    for ( (s,e) <- sorted.keySet.toList.sorted.map(sorted(_).toList).reduce(_++_) ) {
      if ( ( (s == 0) || flags(s-1) == false ) && flags(e+1) == false ) {
        for ( i <- s to e ) {
          flags(i) = true
        }
      }
    }
    return flags.zipWithIndex.filter(_._1).map(_._2).toSet
  }

  def greedyLengthFreq[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] = {
    val sorted = new mutable.HashMap[Seq[T],mutable.Set[(Int,Int)]] with mutable.MultiMap[Seq[T], (Int,Int)]
    for ( ent <- rp ) {
      sorted.addBinding(body.slice(ent._1, ent._2+1), ent)
    }
    val flags = Array.fill(body.size + 2)(false)
    if ( sorted.size == 0 ) {
      return Set()
    }
    for ( (s,e) <- sorted.keySet.toList.sortBy(x => (-sorted(x).size, -x.size)).map(sorted(_).toList).reduce(_++_) ) {
      if ( ( (s == 0) || flags(s-1) == false ) && flags(e+1) == false ) {
        for ( i <- s to e ) {
          flags(i) = true
        }
      }
    }
    return flags.zipWithIndex.filter(_._1).map(_._2).toSet
  }

  def greedyConservative[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] = {
    val flags = Array.fill(body.size + 2)(false)
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

  def greedySliced[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] = {
    import scala.collection.mutable.PriorityQueue
    val flags = Array.fill(body.size + 2)(false)
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
