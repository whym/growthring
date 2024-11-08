/**
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */

package org.whym.growthring

import scala.jdk.CollectionConverters.*
import scala.collection.{mutable, immutable}
import scala.util.boundary, boundary.break
import scala.reflect.ClassTag

/**
  * Helper functions for partial-order multiple sequence alignment
  *
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */
object MultipleSequenceAlignment {

  /**
    * Unit of alignment
    */
  case class Node[T](
      body: immutable.IndexedSeq[T],
      start: Int,
      end: Int,
      freq: Int = 1
  ) {
    def label = body.slice(start, end)
    override def toString = "Node(" + List(
      this.label.toString + "@%X".format(this.body.hashCode),
      start,
      end,
      freq
    ).mkString(", ") + ")"

    def concat(that: Node[T]): Node[T] = {
      if
        this.body.slice(
          this.end,
          this.end + that.label.size
        ) == that.label && this.freq == that.freq
      then {
        Node(this.body, this.start, this.end + that.label.size, this.freq)
      } else if
        that.body.slice(
          that.start - this.label.size,
          that.start
        ) == this.label && this.freq == that.freq
      then {
        Node(that.body, that.start - this.label.size, that.end, that.freq)
      } else {
        throw new RuntimeException("cannot concat: " + this + that)
        // ! TODO: Option にする
      }
    }
  }
}

/**
  * Partial-order multiple sequence alignment of texts
  *
  * @author
  *   Yusuke Matsubara <whym@whym.org>
  */
class MultipleSequenceAlignment[T](strings: Seq[immutable.IndexedSeq[T]]) {
  import MultipleSequenceAlignment.*
  val dags = for (s <- strings) yield {
    Dag(
      0.until(s.size).map(i => Node(s, i, i + 1)),
      0.until(s.size - 1).map(i => (i, i + 1)).toSet
    )
  }

  def weight()(implicit
      eql: Double = 0.0,
      del: Double = 1.0,
      ins: Double = 1.0,
      rep: Double = 1.5,
      ooo: Double = 10.0
  ) =
    (_x: Option[Node[T]], _y: Option[Node[T]]) => {
      (_x, _y) match {
        case (Some(x), Some(y)) => if x.label == y.label then eql else rep
        case (Some(x), _)       => del
        case (_, Some(x))       => ins
        case _                  => ooo
      }
    }

  def align(): Dag[Node[T]] = align(dags)

  private def align(ls: Seq[Dag[Node[T]]]): Dag[Node[T]] = {
    if ls.size == 0 then {
      return Dag(immutable.IndexedSeq(), Set())
    } else if ls.size == 1 then {
      return ls.head
    } else if ls.size == 2 then {
      return ls.head.align(
        ls(1),
        this.weight(),
        x => x.label.toString,
        (x, y) => Node(x.body, x.start, x.end, x.freq + y.freq)
      )
    } else {
      return this.align(ls.slice(0, ls.size / 2)).align(
        this.align(ls.slice(ls.size / 2, ls.size)),
        this.weight(),
        x => x.label.toString,
        (x, y) => Node(x.body, x.start, x.end, x.freq + y.freq)
      )
    }
  }
}

object Dag {
  case class Operation(at: Int, _with: Int, opcode: Operation.OpCode)
  object Operation {
    sealed abstract class OpCode() {
      override def toString(): String = this.getClass.getSimpleName
    }
    object OpEqual extends OpCode
    object OpReplace extends OpCode
    object OpInsert extends OpCode
    object OpDelete extends OpCode
    object OpNone extends OpCode
  }
}

case class Dag[T: ClassTag](
    nodes: immutable.IndexedSeq[T],
    edges: Set[(Int, Int)]
) {
  import Dag.*
  import Dag.Operation.*

  private val _prev_nodes = edges.groupBy { x => x._2 }.map { x =>
    (x._1, x._2.map(_._1))
  }
  private val _next_nodes = edges.groupBy { x => x._1 }.map { x =>
    (x._1, x._2.map(_._2))
  }
  val prev_nodes = Array.tabulate(nodes.size) { i =>
    _prev_nodes.getOrElse(i, Set())
  }
  val next_nodes = Array.tabulate(nodes.size) { i =>
    _next_nodes.getOrElse(i, Set())
  }

  def align[W](
      that: Dag[T],
      weight: (Option[T], Option[T]) => W,
      id: T => String = x => x.toString,
      merge: (T, T) => T = (x, y) => x
  )(implicit num: Numeric[W]): Dag[T] = {

    val maxValue = num.fromInt(Int.MaxValue)

    lazy val table: LazyList[LazyList[(W, Operation, Int, Int)]] =
      LazyList.tabulate(this.nodes.size, that.nodes.size) {
        (this_cur, that_cur) =>
          {
            // println(this_cur, that_cur, this.prev_nodes(this_cur), that.prev_nodes(that_cur)) //!

            val this_prevs = this.prev_nodes(this_cur)
            val that_prevs = that.prev_nodes(that_cur)

            if this_prevs.size == 0 && that_prevs.size == 0 then {
              (
                weight(Some(this.nodes(this_cur)), Some(that.nodes(that_cur))),
                (if id(this.nodes(this_cur)) == id(that.nodes(that_cur)) then {
                   Operation(this_cur, that_cur, OpEqual)
                 } else {
                   Operation(this_cur, that_cur, OpReplace)
                 }),
                -1,
                -1
              )
            } else {
              var min: (W, Operation, Int, Int) =
                (maxValue, Operation(-1, -1, OpNone), -1, -1)
              def update_min(p: (W, Operation, Int, Int)): Unit = {
                if num.lt(p._1, min._1) then {
                  min = p
                }
              }

              for i <- this_prevs do {
                // println("looking for deletes at " + i + "," + that_cur)
                val (score, ops, _, _) = table(i)(that_cur)
                val s =
                  num.plus(score, weight(None, Some(that.nodes(that_cur))))
                update_min((
                  s,
                  Operation(this_cur, that_cur, OpDelete),
                  i,
                  that_cur
                ))
              }
              for i <- that_prevs do {
                // println("looking for inserts at " + List(this_cur, that_cur, i).mkString(",")  + " " + that.nodes + that.edges)
                val (score, ops, _, _) = table(this_cur)(i)
                val s =
                  num.plus(score, weight(Some(this.nodes(this_cur)), None))
                update_min((
                  s,
                  Operation(this_cur, that_cur, OpInsert),
                  this_cur,
                  i
                ))
              }
              for i <- this_prevs; j <- that_prevs do {
                // println("looking for replaces at " + i + "," + j)
                val (score, ops, _, _) = table(i)(j)
                val s = num.plus(
                  score,
                  weight(Some(this.nodes(this_cur)), Some(that.nodes(that_cur)))
                )
                update_min((
                  s,
                  Operation(
                    this_cur,
                    that_cur,
                    if id(this.nodes(this_cur)) == id(that.nodes(that_cur))
                    then {
                      OpEqual
                    } else {
                      OpReplace
                    }
                  ),
                  i,
                  j
                ))
              }

              min
            }
          }
      }

    val pointers = new mutable.ListBuffer[(Int, Int)]
    pointers.append((this.nodes.size - 1, that.nodes.size - 1))
    while
      this.prev_nodes(pointers.head._1).size > 0 || that.prev_nodes(
        pointers.head._2
      ).size > 0
    do {
      val (_, _, i, j) = table(pointers.head._1)(pointers.head._2)
      pointers.prepend((i, j))
    }

    val ops = pointers.map { x => table(x._1)(x._2)._2 }.toList

    // assertions
    if
      ops.head.opcode != OpEqual ||
      ops.last.opcode != OpEqual
    then {
      throw new RuntimeException("needs to start and end with Equal")
    }

    val this_trans = new mutable.HashMap[Int, Int]
    val that_trans = new mutable.HashMap[Int, Int]
    var count = 1
    this_trans(0) = 0
    that_trans(0) = 0
    for Seq(prev, cur) <- ops.sliding(2) do {
      for i <- Range(prev.at + 1, cur.at) do {
        if this_trans.getOrElseUpdate(i, count) == count then {
          count += 1
        }
      }
      for i <- Range(prev._with + 1, cur._with) do {
        if that_trans.getOrElseUpdate(i, count) == count then {
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
        case Operation(_, _, OpNone) => {
          throw new RuntimeException("OpNone")
        }
      }
    }

    // println(this_trans, this.nodes)
    // println(that_trans, that.nodes)

    val n =
      mutable.ArrayBuffer.fill(count)(this.nodes(0))
      // ! TODO: ^ placeholder として不可能な値を使う
    for (i, j) <- this_trans do {
      n(j) = this.nodes(i)
    }
    for (i, j) <- that_trans do {
      if n(j) != this.nodes(0) || i == 0 then {
        n(j) = merge(n(j), that.nodes(i))
      } else {
        n(j) = that.nodes(i)
      }
    }
    val e =
      (for ((i, j) <- this.edges) yield {
        (this_trans(i), this_trans(j))
      }) ++ (for ((i, j) <- that.edges) yield {
        (that_trans(i), that_trans(j))
      }).toSet

    Dag(n.toIndexedSeq, e)
  }

  def trace[S](seq: Seq[S], id: T => String = x => x.toString)(implicit
      id2: S => String = id
  ): Option[List[Int]] = {
    // ! TODO: visited で重複訪問を防ぐ
    def _trace(rseq: Seq[String], acc: List[Int]): Option[List[Int]] =
      if rseq.size == 0 then {
        Some(acc)
      } else {
        val (h, hh) = (acc.head, rseq.head)
        val p = prev_nodes(h).filter(x => id(nodes(x)) == hh)
        if p.size > 0 then {
          _trace(rseq.tail, p.head :: acc)
        } else {
          None
        }
      }
    boundary {
      if seq.size == 0 then {
        Some(List())
      } else {
        val rseq = seq.reverse.map(x => id2(x))
        for i <- 0.until(nodes.size) do {
          if rseq.head == id(nodes(i)) then {
            val r = _trace(rseq.tail, List(i))
            if r != None then break(r)
          }
        }
        None
      }
    }
  }

  def dot(nodeformat: (Int, T) => String = (i, x) =>
    "N_%d[label=\"%s\"];".format(i, x.toString)): List[String] =
    List(
      "digraph g {",
      "  rankdir = LR;"
    ) ++
      (for ((x, i) <- nodes.zipWithIndex) yield {
        " " + nodeformat(i, x)
      }) ++
      edges.map(x => "  N_%d -> N_%d;".format(x._1, x._2)).toList.sorted ++
      List("}")

  def compact(concat: (T, T) => T): Dag[T] = {
    val visited = new mutable.BitSet
    def compactable_edges(
        root: Int,
        buff: List[Int],
        acc: Set[List[Int]]
    ): Set[List[Int]] = {
      val prevs = prev_nodes(root)
      if buff.size == 0 then {
        compactable_edges(root, root :: buff, acc)
      } else if prevs.size == 0 then {
        acc + buff
      } else if prevs.size == 1 then {
        val p = prevs.head
        val nx = next_nodes(p)
        if nx.size == 1 then {
          compactable_edges(
            p,
            p :: buff,
            acc
          )
        } else if visited contains p then {
          acc + buff
        } else {
          visited += p
          compactable_edges(
            p,
            List(),
            acc + buff
          )
        }
      } else {
        Set(buff) ++ (prevs.foldLeft(Set[List[Int]]())((s, x) =>
          s ++ compactable_edges(x, List(), acc)
        ))
      }
    }

    val compactable =
      compactable_edges(nodes.size - 1, List(), Set()).toArray.sortBy(_.head)
    val itrans = new mutable.HashMap[Int, (Int, T)]()
    itrans ++= Map(nodes.zipWithIndex.map(_ match {
      case (n, i) => (i, (i, n))
    })*)

    for ls <- compactable if ls.size >= 2 do {
      // System.err.println(ls, ls.map(nodes(_)).reduce(concat))//!

      val h = ls.head
      val ids = ls.foldLeft(Set[Int]())((s, x) => s + x).toArray.sorted
      val n: T = ids.map(nodes(_)).reduce(concat)
      for i <- ids do {
        itrans(i) = (ids.head, n)
      }
    }
    val itrans_sorted = itrans.toArray.sortBy(_._1)
    val nn = itrans_sorted.map(_._2._2).distinct
    val jtrans = new mutable.HashMap[Int, Int]
    for (i, (j, n)) <- itrans_sorted do {
      jtrans.get(j) match {
        case Some(_) => {}
        case _ => {
          jtrans(i) = jtrans.size
        }
      }
    }
    val ee = edges.map(x =>
      (
        jtrans(itrans(x._1)._1),
        jtrans(itrans(x._2)._1)
      )
    ).filter(x => x._1 != x._2)
    Dag(nn.toIndexedSeq, ee)
  }
}
