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

  def overlaps(x: (Int,Int), y: (Int,Int)) =
    (x != y && ( (x._2 - y._1 + 2) * (x._1 - y._2 - 2) < 0 ))

  def hasOverlap(xx: Iterable[(Int,Int)], yy: Iterable[(Int,Int)]): Boolean = {
    for ( x <- xx; y <- yy; if overlaps(x, y) ) {
      return true
    }
    return false
    //TODO: spans(x._1) = x._2 なる配列を使ったほうが速そう
  }

  def rec(set: Set[Int], rp: Seq[(Int,Int)]): Set[Int] =
    if ( rp.size == 0 ) {
      set
    } else if ( rp.size == 1 ) {
      set ++ Range(rp.head._1, rp.head._2+1).toSet
    } else {
      List(rec(set ++ Range(rp.head._1, rp.head._2+1).toSet,
               rp.filter(x => x._1 > rp.head._2)),
           rec(set,
               rp.tail)
         ).maxBy(_.size)
    }

  def dp[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] =
    rec(Set(), rp)

  def exhaustive3[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] = {
    val remains = new mutable.ListBuffer[(Int,Int)]
    remains.appendAll(rp)
    val cands = new mutable.ListBuffer[mutable.ListBuffer[(Int,Int)]]
    if ( remains.size == 0 ) {
      return Set.empty[Int]
    }
    val h = remains.head
    remains.drop(1)
    def withdrawOverlapping(e: Int): Seq[(Int,Int)] = {
      val ret = new mutable.ListBuffer[(Int,Int)]
      while ( remains.size > 0  &&  remains.head._1 <= e + 1 ) {
        ret.append(remains.head)
        remains.drop(1)
      }
      ret
    }
    for ( r <- (h +: withdrawOverlapping(h._2)) ) {
      cands.append(mutable.ListBuffer(r))
    }
    while ( remains.size > 0 ) {
      val h = remains.head
      val cands2 = new mutable.ListBuffer[mutable.ListBuffer[(Int,Int)]]
      for ( s <- cands ) {
        if ( !overlaps(s.last, h) ) {
          for ( r <- (h +: withdrawOverlapping(h._2)) ) {
            cands2.append(s :+ r)
          }
        }
      }
      cands.appendAll(cands2)
    }
    cands.max(Ordering.by[Iterable[(Int,Int)],Int](x => x.map(y => (1 + y._2 - y._1)).sum)).map(x => Range(x._1,x._2+1).toList).reduce(_++_).toSet
  }

  def exhaustive[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] = {
    val remains = mutable.ListBuffer(rp)
    val cands = new mutable.ListBuffer[mutable.ListBuffer[(Int,Int)]]
    cands.append(mutable.ListBuffer(rp.head))
    cands.append(mutable.ListBuffer(rp.tail.head))
    for ( r <- rp.tail.tail ) {
      val cands2 = new mutable.ListBuffer[mutable.ListBuffer[(Int,Int)]]
      for ( s <- cands ) {
        if ( !overlaps(s.last, r) ) {
          cands2.append(s :+ r)
        }
      }
      cands.appendAll(cands2)
    }
    cands.max(Ordering.by[Iterable[(Int,Int)],Int](x => x.map(y => (1 + y._2 - y._1)).sum)).map(x => Range(x._1,x._2+1).toList).reduce(_++_).toSet
  }

  def exhaustive2[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] =
    rp.toSet.subsets.max(Ordering.by[Set[(Int,Int)],Int](x =>
      if (hasOverlap(x, x)) { Int.MinValue } else { x.map(y => (1 + y._2 - y._1)).sum })).map(x => Range(x._1,x._2+1).toList).reduce(_++_).toSet

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
    val invalidated = new mutable.HashMap[IndexedSeq[T], Int] withDefault (_ => 0)
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
