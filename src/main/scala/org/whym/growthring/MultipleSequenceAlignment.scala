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

object MultipleSequenceAlignment {
  case class Node[T](body: List[T], start: Int, end: Int) {
    def label = body.slice(start, end)
    override def toString = this.label.toString + "@%X".format(this.body.hashCode)
    
    def concat(that: Node[T]): Node[T] = {
      if ( this.body.slice(this.end, this.end + that.label.size) == that.label ) {
        Node(this.body, this.start, this.end + that.label.size)
      } else if ( that.body.slice(that.start - this.label.size, that.start) == this.label ) {
        Node(that.body, that.start - this.label.size, that.end)
      } else {
        throw new RuntimeException("cannot concat: " + this + that)  //! Option にする
      }
    }
  }
}

class MultipleSequenceAlignment[T](strings: List[List[T]]) {
  import MultipleSequenceAlignment._
  val dags = for (s <- strings) yield {
    Dag(0.until(s.size).map(i => Node(s, i, i + 1)).toList,
        0.until(s.size-1).map(i => Pair(i, i+1)).toSet)
  }

  def weight()(implicit
               eql:Double=0.0,
               del:Double=1.0,
               ins:Double=1.0,
               rep:Double=1.5,
               ooo:Double=10.0) =
    (_x: Option[Node[T]], _y: Option[Node[T]]) => {
    (_x, _y) match {
      case (Some(x),Some(y)) => if (x.label == y.label) eql else rep
      case (Some(x),_)    => del
      case (_,Some(x))    => ins
      case _              => ooo
    }
    }

  def align(): Dag[Node[T]] = align(dags)

  // import scala.annotation.tailrec
  // @tailrec
  private def align(ls: List[Dag[Node[T]]]): Dag[Node[T]] = {
    if ( ls.size == 0 ) {
      return Dag(List(), Set())
    } else if ( ls.size == 1 ) {
      return ls(0)
    } else if ( ls.size == 2 ) {
      return ls(0).align(ls(1), this.weight(), x => x.label.toString)
    } else {
      return this.align(ls.slice(0, ls.size/2)).align(this.align(ls.slice(ls.size/2, ls.size)),
                                                      this.weight(),
                                                      x => x.label.toString)
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

  def align[W](that: Dag[T], weight: (Option[T],Option[T]) => W, id: T=>String = x=>x.toString)
  (implicit num: Numeric[W]): Dag[T] = {
    val memo = new mutable.HashMap[(Int,Int), (W, List[Operation])]
    val (score,ops) = this.align_(that, weight, id, memo, this.nodes.length - 1, that.nodes.length - 1)(num)

    //println(memo) //!

    //println("this: " + this.nodes + this.edges)//!
    //println("that: " + that.nodes + that.edges)//!

    //println(score, ops) //!

    // assertions
    if ( ops.head.opcode != OpEqual ||
        ops.last.opcode != OpEqual ) {
      throw new RuntimeException("needs to start and end with Equal")
    }

    def sliding_pairs[T](ls: List[T]): List[(T,T)] = ls.slice(0, ls.size-1) zip ls.slice(1, ls.size)

    val this_trans = new mutable.HashMap[Int,Int]
    val that_trans = new mutable.HashMap[Int,Int]
    var count = 1
    this_trans(0) = 0
    that_trans(0) = 0
    for ((prev,cur) <- sliding_pairs(ops)) {
      for ( i <- Range(prev.at+1, cur.at) ) {
        if ( this_trans.getOrElseUpdate(i, count) == count ) {
          count += 1
        }
      }
      for ( i <- Range(prev._with+1, cur._with) ) {
        if ( that_trans.getOrElseUpdate(i, count) == count ) {
          count += 1
        }
      }
      cur match {
        case Operation(i, j, OpEqual) => {
          this_trans(i) = count
          that_trans(j) = count
          count += 1
        }
        case Operation(i, j, OpReplace) => {
          this_trans(i) = count
          count += 1
          that_trans(j) = count
          count += 1
        }
        case Operation(i, j, OpInsert) => {
          that_trans(j) = count
          count += 1
        }
        case Operation(i, j, OpDelete) => {
          this_trans(i) = count
          count += 1
        }
      }
    }

    // println(this_trans, this.nodes)
    // println(that_trans, that.nodes)

    val n = mutable.ArrayBuffer.fill(count)(this.nodes(0)) //! placeholder として不可能な値を使う
    for ( (i, j) <- this_trans ) {
      if ( n(j) != 0 && id(n(j)) != id(this.nodes(i)) ) {
        //throw new RuntimeException("n() doubly substituted for " + j + " " +List(n(j), this.nodes(i)))
      }
      n(j) = this.nodes(i)
    }
    for ( (i, j) <- that_trans ) {
      if ( n(j) != 0 && id(n(j)) != id(that.nodes(i)) ) {
        //throw new RuntimeException("n() doubly substituted for " + j + " " +List(n(j), that.nodes(i)))
      }
      n(j) = that.nodes(i)
    }

    val e = (for ( (i,j) <- this.edges ) yield {
      Pair(this_trans(i), this_trans(j))
    }) ++ (for ( (i,j) <- that.edges ) yield {
      Pair(that_trans(i), that_trans(j))
    }).toSet

    // println(List(n.toList.map(_.toString), e)) //!
    
    Dag(n.toList, e)
  }

  //! edge の頻度（くっつけたことがあればその回数、なければ1）をカウントする

  def align_[W](that: Dag[T], weight: (Option[T],Option[T]) => W, id: T=>String, memo: mutable.Map[(Int,Int), (W, List[Operation])], this_cur: Int, that_cur: Int)(implicit num: Numeric[W]): (W,List[Operation]) = {
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
                 List(if (id(this.nodes(this_cur)) == id(that.nodes(that_cur))) {
                   Operation(this_cur, that_cur, OpEqual)
                 } else {
                   Operation(this_cur, that_cur, OpReplace)
                 }))
      memo((this_cur, that_cur)) = ret
      return ret
    }

    var min:Option[Pair[W, List[Operation]]] = None
    def update_min(p: Pair[W, List[Operation]]) {
      min = Some(if (min.isEmpty) {
        p
      } else {
        List(min.get, p).min(Ordering.by[(W, List[Operation]), W](_._1))
      })
    }
    def memoise(i:Int, j:Int, value:Pair[W, List[Operation]]) {
      memo((i, j)) = value
    }

    for ( i <- this.prev_nodes(this_cur) ) {
      //println("looking for deletes at " + i + "," + that_cur)
      val (score,ops) = this.align_(that, weight, id, memo, i, that_cur)(num)
      val s = num.plus(score, weight(None, Some(that.nodes(that_cur))))
      update_min((s, ops ++ List(Operation(this_cur, that_cur, OpDelete))))
    }
    for ( i <- that.prev_nodes(that_cur) ) {
      //println("looking for inserts at " + List(this_cur, that_cur, i).mkString(",")  + " " + that.nodes + that.edges)
      val (score,ops) = this.align_(that, weight, id, memo, this_cur, i)(num)
      val s = num.plus(score, weight(Some(this.nodes(this_cur)), None))
      update_min((s, ops ++ List(Operation(this_cur, that_cur, OpInsert))))
    }
    for ( i <- this.prev_nodes(this_cur);
         j <- that.prev_nodes(that_cur) ) yield {
              //println("looking for replaces at " + i + "," + j)
           val (score,ops) = this.align_(that, weight, id, memo, i, j)(num)
           val s = num.plus(score, weight(Some(this.nodes(this_cur)), Some(that.nodes(that_cur))))
           update_min((s, ops ++
                       List(Operation(this_cur, that_cur,
                                      if (id(this.nodes(this_cur)) == id(that.nodes(that_cur))) {
                                        OpEqual
                                      } else {
                                        OpReplace
                                      }))))
         }
    val ret = min.get
    //println(memo)//!

    memoise(this_cur, that_cur, ret)
    return ret
  }

  def trace[S](seq: List[S], id: T=>String = x=>x.toString)(implicit id2:S=>String=id): Option[List[Int]] = {
    def _trace(rseq: List[Int], acc: List[Int]): Option[List[Int]] =
      if (rseq.size == 0) {
        Some(acc)
      } else {
        val (h, hh) = (acc.head, rseq.head)
        val p = prev_nodes(h).filter(x => x == hh)
        if ( p.size > 0 ) {
          _trace(rseq.tail, p.head :: acc)
        } else {
          None
        }
      }
    if (seq.size == 0) {
      Some(List())
    } else {
      val map = Map(nodes.map(id).zipWithIndex: _*)
      if ( seq.filter(x => map.get(id2(x)).isEmpty).size > 0 ) {
        None
      } else {
        val rseq = seq.reverse.map(x => map(id2(x)))
        _trace(rseq.tail, List(rseq.head))
      }
    }
  }

  def dot(id:T=>String = x=>x.toString): List[String] =
    List("digraph g {",
         "  rankdir = LR;") ++
    (for ((x,i) <- nodes.zipWithIndex) yield {
      "  N_%d[label=\"%s\"];".format(i, id(x))
    }) ++
    edges.map(x => "  N_%d -> N_%d;".format(x._1, x._2)).toList.sorted ++
    List("}")

  def compact(concat: (T,T)=>T): Dag[T] = {
    def compactable_edges(root: Int, buff: List[Int], acc: Set[List[Int]]): Set[List[Int]] = {
      val prevs = prev_nodes(root)
      if ( buff.size == 0 ) {
        compactable_edges(root, root :: buff, acc)
      } else if ( prevs.size == 0 ) {
        if ( buff.size > 1 ) {
          acc + buff
        } else {
          acc
        }
      } else if ( prevs.size == 1 ) {
        val p = prevs.head
        if ( next_nodes(p).size == 1 ) {
          compactable_edges(p,
                            p :: buff,
                            acc)
        } else {
          compactable_edges(p,
                            List(),
                            acc + buff)
        }
      } else {
        Set(buff) ++ (prevs.foldLeft(Set[List[Int]]())((s,x) =>
          s ++ compactable_edges(x, List(), acc)))
      }
    }
    val compactable = compactable_edges(nodes.size - 1, List(), Set()).toList.sorted(Ordering.by[List[Int], Int](_.head))
    val itrans = new mutable.HashMap[Int, (Int, T)]
    for ( (n,i) <- nodes.zipWithIndex ) {
      itrans(i) = (i, n) //! まとめて初期化
    }
    for ( ls <- compactable if ls.size >= 2) {
      System.err.println(ls, ls.map(nodes(_)).reduce(concat))//!

      val h = ls.head
      val ids = ls.foldLeft(Set[Int]())((s,x) => s + x).toList.sorted
      val n = ids.map(nodes(_)).reduce(concat)
      for ( i <- ids ) {
        itrans(i) = (ids.head, n)
      }
    }
    val itrans_sorted = itrans.toList.sorted(Ordering.by[(Int, (Int, T)), Int](_._1))
    val nn = itrans_sorted.map(_._2._2).distinct
    val jtrans = new mutable.HashMap[Int, Int]
    for ( (i,(j,n)) <- itrans_sorted ) {
      jtrans.get(j) match {
        case Some(_) => {}
        case _ => {
          jtrans(i) = jtrans.size
        }
      }
    }
    val ee = edges.map(x => (jtrans(itrans(x._1)._1),
                             jtrans(itrans(x._2)._1))).filter(x => x._1 != x._2)
    Dag(nn.toList, ee)
  }

  //! インデックスして速くする
  def prev_nodes(node: Int): Set[Int] =
    this.edges.filter(x => x._2 == node).map(x => x._1)
  def next_nodes(node: Int): Set[Int] =
    this.edges.filter(x => x._1 == node).map(x => x._2)
}

object Main {
  def main(args: Array[String]) {
    import scala.io
    val strings = args.map(io.Source.fromFile(_).getLines.toList).flatMap(x => x).toList
    val msa = new MultipleSequenceAlignment(strings.map(x => ("^"+x+"$").toList))
    val dag = msa.align.compact((x,y) => x.concat(y))
    for ( line <- dag.dot(_.label.map(_.toString).mkString) ) {
      println(line)
    }
  }
}
