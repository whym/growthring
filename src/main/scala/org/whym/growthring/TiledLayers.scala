/*
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.jdk.CollectionConverters._
import scala.collection.mutable

/*
 * Given spans, pack them into smallest layers each of which has no overlaps
 *
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
object TiledLayers {

  abstract class Cell(id: Int) extends Ordered[Cell] {
    protected def getId = this.id
    override def compare(that: Cell) = this.getId - that.getId
  }
  case class Begin() extends Cell(1)
  case class End() extends Cell(2)
  case class Inside() extends Cell(3)
  case class Outside() extends Cell(4)
  case class Single() extends Cell(5)

  def greedyTilingCore[T](body: Array[T], rp: Seq[(Int, Int)]): Seq[Set[Int]] = {
    val starts = Array.fill(body.size)(-1)
    val ends = Array.fill(body.size)(-1)
    for ((r, i) <- rp.zipWithIndex) {
      starts(r._1) = i
    }
    val res = new mutable.BitSet
    for (x <- Range(0, rp.size)) {
      res.add(x)
    }
    var cur = new mutable.BitSet
    val ret = new mutable.ListBuffer[Set[Int]]
    while (res.size > 0) {
      var n = 0
      while (n < starts.size) {
        val i = starts(n)
        if (res(i)) {
          cur += i
          res -= i
          n = rp(i)._2 + 1
        } else {
          n += 1
        }
      }
      ret.append(cur.toSet)
      cur.clear()
    }
    ret.toSeq
  }
  def greedyTiling[T](body: Array[T], rp: Seq[(Int, Int)]): Seq[IndexedSeq[Cell]] = {
    var c = 1
    val cov = greedyTilingCore(body, rp).map {
      s =>
        {
          val ar = Array.fill(body.size)(-1)
          for (
            n <- s;
            r = rp(n)
          ) {
            for (i <- Range.inclusive(r._1, r._2)) {
              ar(i) = c
            }
            c += 1
          }
          (-1 +: -1 +: ar).lazyZip(-1 +: ar :+ -1).lazyZip(ar :+ -1 :+ -1).map {
            case (_, -1, _)  => Outside()
            case (-1, _, -1) => Single()
            case (x, y, z)   => (if (x == y && y == z) { Inside() } else if (x == y) { End() } else if (y == z) { Begin() } else { Single() })
          }.slice(1, body.size + 1).toIndexedSeq
        }
    }.toList
    return cov
  }

}
