/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.annotation.tailrec

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
  def hasOverlap(x: Iterable[(Int,Int)]): Boolean = hasOverlap(x, x)

  def dp_(rp2: Seq[(Int,Int)]): Set[Int] = {
    case class Operation(score: Int, prevPos: Int, skip: Boolean)

    val rp = rp2.toIndexedSeq.sorted

    val index_ = {
      val ret = mutable.ArrayBuffer.fill(rp.last._2 + 1)(-1)
      var i = rp(0)._2 + 2
      var n = 0
      while ( i < ret.size && n < rp.size ) {
        ret(i) = n
        if ( i >= rp(n+1)._2 + 1 ) {
          n += 1
        }
        i += 1
      }
      ret.toIndexedSeq
    }
    def index(i: Int) = if ( i < 0 ) {
      -1
    } else if ( i < index_.size ) {
      index_(i)
    } else {
      index_(rp.size - 1)
    }

    val table = new mutable.ArrayBuffer[Operation]
    table.append(Operation(rp(0)._2 - rp(0)._1 + 1, -1, false))
    for ( n <- 1 until rp.size ) {
      val s = rp(n)._2 - rp(n)._1 + 1
      val p  = table(n-1)
      val prevPos = index(rp(n)._1)
      table.append({
        if ( prevPos >= 0 ) {
          val ps = table(prevPos).score
          if ( prevPos == n-1 ) {
            Operation(ps + s, prevPos, false)
          } else {
            if ( ps + s > p.score ) {
              Operation(ps + s, prevPos, false)
            } else {
              Operation(p.score, n-1, true)
            }
          }
        } else{
          if ( p.score > s ) {
            Operation(p.score, n-1, true)
          } else {
            Operation(s, -1, false)
          }
        }
      })
    }
    val m = table.zipWithIndex.maxBy(_._1.score)._2
    val ret = new mutable.HashSet[Int]
    var i = m
    while ( i >= 0 ) {
      if ( !table(i).skip ) {
        ret ++= Range(rp(i)._1, rp(i)._2+1).toSet
      }
      i = table(i).prevPos
    }
    ret.toSet
  }

  def dp[T](body: Array[T], rp: Seq[(Int,Int)]) =
    dp_(rp)

  def exhaustive_[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] = {
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

  def exhaustive[T](body: Array[T], rp: Seq[(Int,Int)]): Set[Int] = {
    rp.toSet.subsets.max(
      Ordering.by[Set[(Int,Int)],Int] {
        set =>
          if (hasOverlap(set)) {
            Int.MinValue
          } else {
            set.toList.map(x => (1 + x._2 - x._1)).sum
          }
      }
    ).map(x => Range(x._1,x._2+1).toSet).reduce(_++_)
  }

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
