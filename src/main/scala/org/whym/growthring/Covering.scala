/**
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */

package org.whym.growthring

import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import scala.collection.IndexedSeqView
import scala.annotation.tailrec
import scala.util.boundary, boundary.break

/**
  * Given spans, fill the parent sequence with some of the spans so that no
  * overlap nor concatenation happens
  *
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */
object Covering {

  def overlaps(x: (Int, Int), y: (Int, Int)) =
    (x != y && ((x._2 - y._1 + 2) * (x._1 - y._2 - 2) < 0))

  def hasOverlap(xx: Iterable[(Int, Int)], yy: Iterable[(Int, Int)]): Boolean =
    boundary:
      for x <- xx; y <- yy; if overlaps(x, y)
      do break(true)
      false
    // TODO: spans(x._1) = x._2 なる配列を使ったほうが速そう

  def hasOverlap(x: Iterable[(Int, Int)]): Boolean = hasOverlap(x, x)

  def dp_(rp2: Seq[(Int, Int)], gap: Int): Set[Int] = {
    case class Operation(score: Int, prevPos: Int, skip: Boolean)

    if rp2.size == 0 then {
      return Set()
    }
    val rp = rp2.toIndexedSeq.sorted

    // index(n) は rp(n) と重複しない rp(i) のうち最大の i を返す
    val index_ = {
      val ret = mutable.ArrayBuffer.fill(rp.last._2 + 1)(-1)
      var i = rp(0)._2 + 1 + gap
      var n = 0
      while i < ret.size && n < rp.size do {
        ret(i) = n
        if i >= rp(n + 1)._2 + gap then {
          n += 1
        }
        i += 1
      }
      ret.toIndexedSeq
    }
    def index(i: Int) = if i < 0 then {
      -1
    } else if i < index_.size then {
      index_(i)
    } else {
      index_(rp.size - 1)
    }

    // table(n) は _.skip=false のとき rp(n) を使ってそれ以前の rp でとりうる最大のスコアとそのときに使う直前の rp(_.prevPos) なる prevPos を保持する。_.skip=true のとき、 rp(n) を使わずにそれ以前の rpでとりうる最大のスコアを保持する。
    val table = new mutable.ArrayBuffer[Operation]
    table.append(Operation(rp(0)._2 - rp(0)._1 + 1, -1, false))
    for n <- 1 until rp.size do {
      val s = rp(n)._2 - rp(n)._1 + 1 // size
      val p = table(n - 1) // previous
      val prevPos = index(rp(n)._1)
      table.append({
        if prevPos >= 0 then {
          val newscore = table(prevPos).score + s
          if prevPos == n - 1 || newscore > p.score then {
            Operation(newscore, prevPos, false)
          } else {
            Operation(p.score, n - 1, true)
          }
        } else if p.score > s then {
          Operation(p.score, n - 1, true)
        } else {
          Operation(s, -1, false)
        }
      })
    }
    val m = table.zipWithIndex.maxBy(_._1.score)._2
    val ret = new mutable.BitSet
    var i = m
    while i >= 0 do {
      if !table(i).skip then {
        ret ++= Range.inclusive(rp(i)._1, rp(i)._2).toSet
      }
      i = table(i).prevPos
    }
    ret.toSet
  }

  def dp[T](body: Array[T], rp: Seq[(Int, Int)], gap: Int = 1) =
    dp_(rp, gap)

  def exhaustive[T](
      body: Array[T],
      rp: Seq[(Int, Int)],
      gap: Int = 1
  ): Set[Int] =
    rp.toSet.subsets().max(
      Ordering.by[Set[(Int, Int)], Int] {
        set =>
          if hasOverlap(set) then {
            Int.MinValue
          } else {
            set.toList.map(x => (gap + x._2 - x._1)).sum
          }
      }
    ).map(x => Range.inclusive(x._1, x._2).toSet).flatten

  def greedyLength[T](
      body: Array[T],
      rp: Seq[(Int, Int)],
      gap: Int = 1
  ): Set[Int] = {
    if gap != 0 && gap != 1 then {
      throw new IllegalArgumentException("gap must be 1 or 0:" + gap)
    }

    // sort by length
    val sorted = mutable.MultiDict[Int, (Int, Int)]()
    for ent <- rp do {
      sorted += ((ent._1 - ent._2, ent))
    }

    val flags = new mutable.BitSet
    if sorted.size == 0 then {
      return Set()
    }
    for
      (s, e) <- sorted.keySet.toList.sorted.map(sorted.get(_).toList).flatten
    do {
      if
        ((s == 0) || (gap == 0 | flags(s - 1) == false)) &&
        flags(s) == false && flags(e) == false &&
        (gap == 0 | flags(e + 1) == false)
      then {
        for i <- s to e do {
          flags(i) = true
        }
      }
    }
    return flags.toSet
  }

  def greedyLengthFreq[T](
      body: Array[T],
      rp: Seq[(Int, Int)],
      gap: Int = 1
  ): Set[Int] = {
    if gap != 0 && gap != 1 then {
      throw new IllegalArgumentException("gap must be 1 or 0:" + gap)
    }

    val sorted = mutable.MultiDict[IndexedSeqView[T], (Int, Int)]()
    for ent <- rp do {
      sorted += ((body.view.slice(ent._1, ent._2 + 1), ent))
    }
    val flags = new mutable.BitSet
    if sorted.size == 0 then {
      return Set()
    }
    for
      (s, e) <- sorted.keySet.toList.sortBy(x =>
        (-sorted.get(x).size, -x.size)
      ).map(sorted.get(_).toList).flatten
    do {
      if
        ((s == 0) || (gap == 0 | flags(s - 1) == false)) &&
        flags(s) == false && flags(e) == false
        && (gap == 0 | flags(e + 1) == false)
      then {
        for i <- s to e do {
          flags(i) = true
        }
      }
    }
    return flags.toSet
  }

  def greedyConservative[T](body: Array[T], rp: Seq[(Int, Int)]): Set[Int] = {
    val flags = new mutable.BitSet
    val invalidated =
      new mutable.HashMap[IndexedSeq[T], Int] withDefault (_ => 0)
    val groups = rp.groupBy(x => body.view.slice(x._1, x._2 + 1).toIndexedSeq)
    val min_freq = groups.map(_._2.size).min
    for (seg, ls) <- groups.toList.sortBy(x => (-x._1.size, x._2.size)) do {
      val checks = for ((s, e) <- ls)
        yield (((s == 0) || flags(s - 1) == false) && flags(e + 1) == false)
      if checks.reduce(_ && _) && ls.size - invalidated(seg) >= min_freq then {
        for (s, e) <- ls; i <- s to e do {
          flags(i) = true
        }
        for i <- 0 until seg.size; j <- i + 1 until seg.size do {
          invalidated(seg.slice(i, j)) += 1
        }
      }
    }
    return flags.toSet
  }

  def greedySliced[T](
      body: Array[T],
      rp: Seq[(Int, Int)],
      gap: Int = 1
  ): Set[Int] = {
    if gap != 0 && gap != 1 then {
      throw new IllegalArgumentException("gap must be 1 or 0:" + gap)
    }
    import scala.collection.mutable.PriorityQueue
    val flags = new mutable.BitSet
    val queue =
      new PriorityQueue[(Int, Int)]()(Ordering.by[(Int, Int), Int](x =>
        x._2 - x._1
      ))
    for x <- rp do {
      queue.enqueue(x)
    }

    def closestAvailable(span: (Int, Int)): (Int, Int) = {
      val start = (span._1 to span._2).find(i =>
        (i > 0 && flags(i - 1) == false) || (i == 0 && flags(0) == false)
      )
      val end = (span._1 to span._2).reverse.find(i => flags(i + 1) == false)
      (
        start match { case Some(x) => x; case _ => span._1 },
        end match { case Some(x) => x; case _ => span._2 }
      )
    }

    while queue.size > 0 do {
      val h = queue.dequeue()
      val (s, e) = h
      if
        ((s == 0) || (gap == 0 | flags(s - 1) == false)) &&
        flags(s) == false && flags(e) == false &&
        (gap == 0 | flags(e + 1) == false)
      then {
        for i <- s to e do {
          flags(i) = true
        }
      } else {
        // slice and enqueue
        val n = closestAvailable((s, e))
        if n != (s, e) && n._2 >= n._1 then {
          queue.enqueue(n)
        }
      }
    }
    return flags.toSet
  }

  // observe that the covering problem can be casted into a max-weight matching problem by translating spans into vertices.
  // vertex weights are given by span length, edge weights are given by max of the two end-points.
  // use algorithms to find optimals (slow), or approximate
  // def optimal(rp: Seq[(Int,Int)]): Set[Int]
  // def nearOptimal(rp: Seq[(Int,Int)]): Set[Int]
}
