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
    val edges = new mutable.HashSet[(Int, Int)]
    for ( i <- 0.until(s.size) ) {
      nodes += Node(s, i, i + 1)
      edges += Pair(i, i+1)
    }
    Dag(nodes.toList, edges.toSet)
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

//! align 関数は object Dag の中に置いたほうがいい

case class Dag[T](nodes: List[T], edges: Set[(Int,Int)]) {

  //! OpCode は内部クラスにする
  case class Operation(at: Int, _with: Int, opcode: OpCode)
  abstract class OpCode() {
    override def toString(): String = this.getClass.getSimpleName
  }
  object OpEqual extends OpCode
  object OpReplace extends OpCode
  object OpInsert extends OpCode
  object OpDelete  extends OpCode
  object OpNone extends OpCode

  //! weight として一括するだけでなく、replace などを個別に与える align も用意
  def align[W](that: Dag[T], weight: (Option[T],Option[T]) => W)(implicit num: Numeric[W]): Dag[T] = {
    val memo = new mutable.HashMap[(Int,Int), (W, List[Operation])]
    val (score,ops) = align(that, weight, memo, this.nodes.length - 1, that.nodes.length - 1)(num)

    //println(memo) //!

    println(score, ops) //!

    // assertions
    if ( ops.head.opcode != OpEqual ||
        ops.reverse.head.opcode != OpEqual ) {
      throw new RuntimeException("needs to start and end with Equal")
    }

    val this_trans = new mutable.HashMap[Int,Int]
    val that_trans = new mutable.HashMap[Int,Int]
    var this_offset = 0
    var that_offset = 0
    var this_cur = 0
    var that_cur = 0
    var prev:OpCode = OpNone
    for ( op <- ops ) {
      while ( this_cur < op.at ) {
        this_trans.get(this_cur) match {
          case Some(_) => {}
          case _ => {
            this_trans(this_cur) = this_cur + this_offset
            if (prev != OpInsert)
              that_offset += 1
          }
        }
        this_cur += 1
      }
      while ( that_cur < op._with ) {
        that_trans.get(that_cur) match {
          case Some(_) => {}
          case _ => {
            that_trans(that_cur) = that_cur + that_offset
            if (prev != OpDelete)
              this_offset += 1
          }
        }
        that_cur += 1
      }
      println(op, this_cur, that_cur, this_offset, that_offset, this_trans, that_trans) //!
      prev = op.opcode
      op match {
        case Operation(i, j, OpEqual) => {
          this_trans(i) = i + this_offset
          that_trans(j) = this_trans(i)
         //this_trans(j) = j + that_offset //! 何らかの形でポインタを保持してどこから合流したかを記憶させる？
          
        }
        case Operation(i, j , OpReplace) => {
          this_trans(i) = i + this_offset
          this_offset += 1
          that_offset += 1
          that_trans(j) = j + that_offset
        }
        case Operation(i, j, OpInsert) => {
          that_trans(j) = j + that_offset
          this_offset += 1
        }
        case Operation(i, j, OpDelete) => {
          this_trans(i) = i + this_offset
          that_offset += 1
        }
      }
      println(op, this_cur, that_cur, this_offset, that_offset, this_trans, that_trans) //!
    }

    println(this_trans)
    println(that_trans)

    val n = mutable.ArrayBuffer.fill((this_trans ++ that_trans).map(x => x._2).max + 1)(this.nodes(0)) //! placeholder として不可能な値を使う
    for ( (i, j) <- this_trans ) {
      n(j) = this.nodes(i)
    }
    for ( (i, j) <- that_trans ) {
      n(j) = that.nodes(i)
    }
    val e = new mutable.HashSet[(Int,Int)]
    for ( (i,j) <- this.edges ) {
      e += Pair(this_trans(i), this_trans(j))
    }
    for ( (i,j) <- that.edges ) {
      e += Pair(that_trans(i), that_trans(j))
    }

    Dag(n.toList, e.toSet)
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
                   Operation(this_cur, that_cur, OpEqual)
                 } else {
                   Operation(this_cur, that_cur, OpReplace)
                 }))
      memo((this_cur, that_cur)) = ret
      return ret
    }
    val deletes = for ( i <- this.prev_nodes(this_cur) ) yield {
      //println("looking for deletes at " + i + "," + that_cur)
      val (score,ops) = this.align(that, weight, memo, i, that_cur)
      val s = num.plus(score, weight(None, Some(that.nodes(that_cur))))
      (s, ops ++ List(Operation(this_cur, i, OpDelete)))
    }
    val inserts = for ( i <- that.prev_nodes(that_cur) ) yield {
      //println("looking for inserts at " + this_cur + "," + i)
      val (score,ops) = this.align(that, weight, memo, this_cur, i)
      val s = num.plus(score, weight(Some(this.nodes(this_cur)), None))
      (s, ops ++ List(Operation(i, that_cur, OpInsert)))
    }
    var substs =
      for ( i <- this.prev_nodes(this_cur);
            j <- that.prev_nodes(that_cur) ) yield {
              //println("looking for replaces at " + i + "," + j)
              val (score,ops) = this.align(that, weight, memo, i, j)
              val s = num.plus(score, weight(Some(this.nodes(this_cur)), Some(that.nodes(that_cur))))
              (s, ops ++ List(Operation(this_cur, that_cur,
                                        if (this.nodes(this_cur) == that.nodes(that_cur)) {
                                          OpEqual
                                        } else {
                                          OpReplace
                                        })))
            }
    val ls = inserts ++ deletes ++ substs
    val ret = if (ls.size > 0 ) ls.min(Ordering.by[(W, List[Operation]), W](_._1)) else (num.zero, List())
    memo((this_cur, that_cur)) = ret
    //println(memo)//!

    return ret
  }

  //! インデックスして速くする
  def prev_nodes(node: Int): Set[Int] = {
    //printf("prev_nodes(%s): %s\n", node, this.edges.filter(x => x._2 == node).map(x => x._1))
    this.edges.filter(x => x._2 == node).map(x => x._1)
  }
}

object Main {
  def main(args: Array[String]) {
  }
}
