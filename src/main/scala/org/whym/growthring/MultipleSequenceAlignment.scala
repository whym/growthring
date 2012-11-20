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
  case class Node(body: List[T], start: Int, end: Int)
  for (s <- strings) {
    val nodes = new mutable.ListBuffer[Node]
    val edges = new mutable.ListBuffer[(Int, Int)]
    for ( i <- 0.until(s.size) ) {
      nodes.append(Node(s, i, i + 1))
      edges.append((i, i+1))
    }
    val dag = Dag[Node](nodes.toList, edges.toList)
  }
}

case class Dag[T](nodes: List[T], edges: List[(Int,Int)]) {
  def align(that: Dag[T]): Dag[T] = {
    return this
  }
}

case class Node(previous: List[Node], next: List[Node])

object Node {
  val nullNode = Node(Nil, Nil)
}

// dag をどう表現すべきか？ 単一のツリーではないので、1つの Node につらなるようにはできない

object Main {
  def main(args: Array[String]) {
    import Node._
    println(Node(nullNode::Nil, List(Node(nullNode::Nil, nullNode::Nil))))
  }
}
