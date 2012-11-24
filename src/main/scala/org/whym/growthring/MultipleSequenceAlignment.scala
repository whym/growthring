/**
 * Partial-order multiple sequence alignment of texts
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
 * DESCRIBE THIS CLASS HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */
class MultipleSequenceAlignment[T](strings: List[List[T]]) {
  case class Node(body: List[T], start: Int, end: Int) {
    def label = body.slice(start, end)
  }
  val dags = for (s <- strings) yield {
    val nodes = new mutable.ListBuffer[Node]
    val edges = new mutable.ListBuffer[(Int, Int)]
    for ( i <- 0.until(s.size) ) {
      nodes.append(Node(s, i, i + 1))
      edges.append((i, i+1))
    }
    Dag(nodes.toList, edges.toList)
  }

  private def weight(x: Option[Node], y:Option[Node]): Double = {
    1.0 //!
  }

  def align(): Dag[Node] = align(dags)

  private def align(ls: List[Dag[Node]]): Dag[Node] = {
    if ( ls.size == 1 ) {
      return ls(0)
    } else if ( ls.size == 2 ) {
      return ls(0).align(ls(1), weight)
    } else {
      return this.align(ls.slice(0, ls.size/2)).align(this.align(ls.slice(ls.size/2, ls.size)), weight)
    }
  }
}

case class Dag[T](nodes: List[T], edges: List[(Int,Int)]) {
  abstract class Operation {
  }
  case class OpEqual(at: Int, _with: Int) extends Operation
  case class OpReplace(at: Int, _with: Int) extends Operation
  case class OpInsert(at: Int, _with: Int)  extends Operation
  case class OpDelete(at: Int)  extends Operation

  //! weight として一括するだけでなく、replace などを個別に与える align も用意
  def align[W](that: Dag[T], weight: (Option[T],Option[T]) => W)(implicit num: Numeric[W]): Dag[T] = {
    val memo = new mutable.HashMap[(Int,Int), (W, List[Operation])]
    val ops = align(that, weight, memo, this.nodes.length - 1, that.nodes.length - 1)(num)

    // 操作列から、統合された dag をつくる
    // while 
    //  if Eq
    //    前の Eq からの部分 dag ペアを並列につなげて合流させる
    //  else
    //    並列して部分 dag ペアをそれぞれたどってためていく

    println(memo) //!
    println(ops) //!

    this //!
  }

  //! edge の頻度（くっつけたことがあればその回数、なければ1）をカウントする

  def align[W](that: Dag[T], weight: (Option[T],Option[T]) => W, memo: mutable.Map[(Int,Int), (W, List[Operation])], this_cur: Int, that_cur: Int)(implicit num: Numeric[W]): (W,List[Operation]) = {
    val maxValue = num.fromInt(Int.MaxValue)

    memo.get((this_cur, that_cur)) match {
      case Some(x) => {
        return x
      }
      case _ => {}
    }
    //println(List(this, that, this_cur, that_cur).mkString(" ")) //!

    if ( this_cur == 0 && that_cur == 0 ) {
      val ret = (weight(Some(this.nodes(this_cur)), Some(that.nodes(that_cur))),
                 List(if (this.nodes(this_cur) == that.nodes(that_cur)) {
                   OpEqual(this_cur, that_cur)
                 } else {
                   OpReplace(this_cur, that_cur)
                 }))
      memo((this_cur, that_cur)) = ret
      return ret
    }
    val deletes: List[(W, List[Operation])] = for ( i <- this.prev_nodes(this_cur) ) yield {
      //println("looking for deletes at " + i + "," + that_cur)
      val (score,ops) = this.align(that, weight, memo, i, that_cur)
      val s = num.plus(score, weight(None, Some(that.nodes(that_cur))))
      (s, ops ++ List(OpDelete(this_cur)))
    }
    val inserts: List[(W, List[Operation])] = for ( i <- that.prev_nodes(that_cur) ) yield {
      //println("looking for inserts at " + this_cur + "," + i)
      val (score,ops) = this.align(that, weight, memo, this_cur, i)
      val s = num.plus(score, weight(Some(this.nodes(this_cur)), None))
      (s, ops ++ List(OpInsert(i, that_cur)))
    }
    var substs: List[(W, List[Operation])] =
      for ( i <- this.prev_nodes(this_cur);
            j <- that.prev_nodes(that_cur) ) yield {
              //println("looking for replaces at " + i + "," + j)
              val (score,ops) = this.align(that, weight, memo, i, j)
              val s = num.plus(score, weight(Some(this.nodes(this_cur)), Some(that.nodes(that_cur))))
              (s, ops ++ List(if (this.nodes(this_cur) == that.nodes(that_cur)) {
                OpEqual(this_cur, that_cur)
              } else {
                OpReplace(this_cur, that_cur)
              }))
            }
    val ls = inserts ++ deletes ++ substs
    val ret = if (ls.length > 0 ) ls.min(Ordering.by[(W, List[Operation]), W](_._1)) else (num.zero, List())
    memo((this_cur, that_cur)) = ret
    //println(memo)//!

    return ret
  }

  //! インデックスして速くする
  def prev_nodes(node: Int): List[Int] = {
    //printf("prev_nodes(%s): %s\n", node, this.edges.filter(x => x._2 == node).map(x => x._1))
    this.edges.filter(x => x._2 == node).map(x => x._1)
  }
}

object Main {
  def main(args: Array[String]) {
  }
}
