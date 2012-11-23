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

  def scorer(x: Node, y:Node): Int = {
    1 //!
  }

  def align(): Dag[Node] = align(dags)

  def align(ls: List[Dag[Node]]): Dag[Node] = {
    if ( ls.size == 1 ) {
      return ls(0)
    } else if ( ls.size == 2 ) {
      return ls(0).align(ls(1), scorer)
    } else {
      return this.align(ls.slice(0, ls.size/2)).align(this.align(ls.slice(ls.size/2, ls.size)), scorer)
    }
  }
}

case class Dag[T](nodes: List[T], edges: List[(Int,Int)]) {
  abstract class Operation {
  }
  case class OpEquals(from: Int, to: Int)       extends Operation
  case class OpSubstitutes(from: Int, to: Int)  extends Operation

  def length = nodes.length

  def align(that: Dag[T], scorer: (T,T) => Int): Dag[T] = {
    val ops = align(that, scorer, this.length, that.length)

    // 操作列から、統合された dag をつくる
    // while 
    //  if Eq
    //    前の Eq からの部分 dag ペアを並列につなげて合流させる
    //  else
    //    並列して部分 dag ペアをそれぞれたどってためていく
    this //!
  }
  def align(that: Dag[T], scorer: (T,T) => Int, this_end: Int, that_end: Int): (Int,List[Operation]) = {
    if ( 0 == this_end ) {
      
    }
    if ( 0 == that_end ) {
      
    }
    val inserts: List[(Int, List[Operation])] = for ( i <- prev_nodes(this_end) ) yield {
      (Int.MaxValue, List(OpEquals(0, 0))) //!
    }
    val deletes: List[(Int, List[Operation])] = for ( i <- prev_nodes(this_end) ) yield {
      (Int.MaxValue, List(OpEquals(0, 0))) //!
    }
    var substs: List[(Int, List[Operation])] =
      for ( i <- prev_nodes(this_end);
            j <- prev_nodes(that_end) ) yield {
              val (score,ops) = this.align(that, scorer, i, j)
              val s = score + scorer(this.nodes(i), that.nodes(j))
              val o = if (this.nodes(i) == that.nodes(j)) {
                OpSubstitutes(i, j)
              } else {
                OpEquals(i, j)
              }
              (s, ops ++ List(o))
            }
    return (inserts ++ deletes ++ substs).min(Ordering.by[(Int, List[Operation]), Int](_._1))
  }
  def prev_nodes(node: Int): List[Int] = {
    this.edges.filter(x => x == node).map(x => x._1)
  }
}

object Main {
  def main(args: Array[String]) {
  }
}
