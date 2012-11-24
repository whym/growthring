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
  val dags: List[Dag[Node]] = for (s <- strings) yield {
    val nodes = new mutable.ListBuffer[Node]
    val edges = new mutable.ListBuffer[(Int, Int)]
    for ( i <- 0.until(s.size) ) {
      nodes.append(Node(s, i, i + 1))
      edges.append((i, i+1))
    }
    Dag[Node](nodes.toList, edges.toList)
  }

  private def scorer(x: Node, y:Node): Int = {
    1 //!
  }

  def align(): Dag[Node] = align(dags)

  private def align(ls: List[Dag[Node]]): Dag[Node] = {
    if ( ls.size == 1 ) {
      return ls(0)
    } else if ( ls.size == 2 ) {
      return ls(0).align(ls(1), scorer)
    } else {
      return this.align(ls.slice(0, ls.size/2)).align(this.align(ls.slice(ls.size/2, ls.size)), scorer)
    }
  }
}

//! scorer は weight として、Dag[T,W] と型パラメータにする
case class Dag[T](nodes: List[T], edges: List[(Int,Int)]) {
  abstract class Operation {
  }
  case class OpEquals(from: Int, to: Int)       extends Operation
  case class OpSubstitutes(from: Int, to: Int)  extends Operation
  case class OpInserts(from: Int)  extends Operation
  case class OpDeletes(from: Int)  extends Operation

  def align(that: Dag[T], scorer: (T,T) => Int): Dag[T] = {
    val memo = new mutable.HashMap[(Int,Int), (Int, List[Operation])]
    val ops = align(that, scorer, memo, this.nodes.length - 1, that.nodes.length - 1)

    // 操作列から、統合された dag をつくる
    // while 
    //  if Eq
    //    前の Eq からの部分 dag ペアを並列につなげて合流させる
    //  else
    //    並列して部分 dag ペアをそれぞれたどってためていく

    println(ops)

    this //!
  }
  def align(that: Dag[T], scorer: (T,T) => Int, memo: mutable.Map[(Int,Int), (Int, List[Operation])], this_cur: Int, that_cur: Int): (Int,List[Operation]) = {
    memo.get((this_cur, that_cur)) match {
      case Some(x) => {
        return x
      }
      case _ => {}
    }
    println(this + " " + this_cur + " " + that_cur)
    if ( -1 == this_cur ) {
      return (Int.MaxValue, List())
    }
    if ( -1 == that_cur ) {
      return (Int.MaxValue, List())
    }
    val inserts: List[(Int, List[Operation])] = for ( i <- prev_nodes(this_cur) ) yield {
      val (score,ops) = this.align(that, scorer, memo, i, that_cur)
      val s = score + scorer(this.nodes(i), that.nodes(that_cur))
      (s, List(OpInserts(i)))
    }
    val deletes: List[(Int, List[Operation])] = for ( i <- prev_nodes(that_cur) ) yield {
      val (score,ops) = this.align(that, scorer, memo, this_cur, i)
      val s = score + scorer(this.nodes(this_cur), that.nodes(i))
      (s, List(OpDeletes(i)))
    }
    var substs: List[(Int, List[Operation])] =
      for ( i <- prev_nodes(this_cur);
            j <- prev_nodes(that_cur) ) yield {
              val (score,ops) = this.align(that, scorer, memo, i, j)
              val s = score + scorer(this.nodes(i), that.nodes(j))
              val o = if (this.nodes(i) == that.nodes(j)) {
                OpSubstitutes(i, j)
              } else {
                OpEquals(i, j)
              }
              (s, ops ++ List(o))
            }
    val ret = (inserts ++ deletes ++ substs ++ List((Int.MaxValue, List(OpEquals(-1,-1))))).min(Ordering.by[(Int, List[Operation]), Int](_._1))
    memo((this_cur, that_cur)) = ret
    return ret
  }
  def prev_nodes(node: Int): List[Int] = {
    printf("prev_nodes(%s): %s\n", node, this.edges.filter(x => x._2 == node).map(x => x._1))
    this.edges.filter(x => x._2 == node).map(x => x._1)
  }
}

object Main {
  def main(args: Array[String]) {
  }
}
