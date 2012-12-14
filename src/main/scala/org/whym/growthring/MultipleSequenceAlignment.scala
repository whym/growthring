/**
 * Partial-order multiple sequence alignment of texts
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.{mutable, immutable}

/**
 * DESCRIBE THIS CLASS HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 */

object MultipleSequenceAlignment {
  case class Node[T](body: immutable.IndexedSeq[T], start: Int, end: Int, freq: Int = 1) {
    def label = body.slice(start, end)
    override def toString = this.label.toString + "@%X".format(this.body.hashCode)
    
    def concat(that: Node[T]): Node[T] = {
      if ( this.body.slice(this.end, this.end + that.label.size) == that.label && this.freq == that.freq ) {
        Node(this.body, this.start, this.end + that.label.size, this.freq)
      } else if ( that.body.slice(that.start - this.label.size, that.start) == this.label && this.freq == that.freq) {
        Node(that.body, that.start - this.label.size, that.end, that.freq)
      } else {
        throw new RuntimeException("cannot concat: " + this + that)  //! Option にする
      }
    }
  }
}

class MultipleSequenceAlignment[T](strings: List[immutable.IndexedSeq[T]]) {
  import MultipleSequenceAlignment._
  val dags = for (s <- strings) yield {
    Dag(0.until(s.size).map(i => Node(s, i, i + 1)),
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

  private def align(ls: Seq[Dag[Node[T]]]): Dag[Node[T]] = {
    if ( ls.size == 0 ) {
      return Dag(immutable.IndexedSeq(), Set())
    } else if ( ls.size == 1 ) {
      return ls(0)
    } else if ( ls.size == 2 ) {
      return ls(0).align(ls(1),
                         this.weight(),
                         x => x.label.toString,
                         (x,y) => Node(x.body, x.start, x.end, x.freq + y.freq))
    } else {
      return this.align(ls.slice(0, ls.size/2)).align(this.align(ls.slice(ls.size/2, ls.size)),
                                                      this.weight(),
                                                      x => x.label.toString,
                                                      (x,y) => Node(x.body, x.start, x.end, x.freq + y.freq))
    }
  }
}

object Dag {
  case class Operation(at: Int, _with: Int, opcode: Operation.OpCode)
  object Operation {
    sealed abstract class OpCode() {
      override def toString(): String = this.getClass.getSimpleName
    }
    object OpEqual   extends OpCode
    object OpReplace extends OpCode
    object OpInsert  extends OpCode
    object OpDelete  extends OpCode
    object OpNone    extends OpCode
  }
}

case class Dag[T](nodes: immutable.IndexedSeq[T], edges: Set[(Int,Int)]) {
  import Dag._
  import Dag.Operation._

  private val _prev_nodes = edges.groupBy{x => x._2}.map{x => (x._1, x._2.map(_._1))}
  private val _next_nodes = edges.groupBy{x => x._1}.map{x => (x._1, x._2.map(_._2))}
  val prev_nodes = Array.tabulate(nodes.size){ i => _prev_nodes.getOrElse(i, Set()) }
  val next_nodes = Array.tabulate(nodes.size){ i => _next_nodes.getOrElse(i, Set()) }

  def align[W](that: Dag[T], weight: (Option[T],Option[T]) => W, id: T=>String = x=>x.toString, merge: (T,T)=>T = (x,y)=>x)
  (implicit num: Numeric[W]): Dag[T] = {

    val maxValue = num.fromInt(Int.MaxValue)

    lazy val table: Stream[Stream[(W, Operation, Int, Int)]] = Stream.tabulate(this.nodes.size, that.nodes.size) { (this_cur, that_cur) => {
      //println(this_cur, that_cur, this.prev_nodes(this_cur), that.prev_nodes(that_cur)) //!

      //! edge の頻度（くっつけたことがあればその回数、なければ1）をカウントする
      val this_prevs = this.prev_nodes(this_cur)
      val that_prevs = that.prev_nodes(that_cur)
        
      if ( this_prevs.size == 0 && that_prevs.size == 0 ) {
        (weight(Some(this.nodes(this_cur)), Some(that.nodes(that_cur))),
         (if (id(this.nodes(this_cur)) == id(that.nodes(that_cur))) {
           Operation(this_cur, that_cur, OpEqual)
         } else {
           Operation(this_cur, that_cur, OpReplace)
         }), -1, -1)
      } else {
        var min: Option[(W, Operation, Int, Int)] = None
        def update_min(p: (W, Operation, Int, Int)) {
          min = Some(if (min.isEmpty || num.lt(p._1, min.get._1) ) {
            p
          } else {
            min.get
          })
        }

        for ( i <- this_prevs ) {
          //println("looking for deletes at " + i + "," + that_cur)
          val (score,ops,_,_) = table(i)(that_cur)
          val s = num.plus(score, weight(None, Some(that.nodes(that_cur))))
          update_min((s, Operation(this_cur, that_cur, OpDelete), i, that_cur))
        }
        for ( i <- that_prevs ) {
          //println("looking for inserts at " + List(this_cur, that_cur, i).mkString(",")  + " " + that.nodes + that.edges)
          val (score,ops,_,_) = table(this_cur)(i)
          val s = num.plus(score, weight(Some(this.nodes(this_cur)), None))
          update_min((s, Operation(this_cur, that_cur, OpInsert), this_cur, i))
        }
        for ( i <- this_prevs; j <- that_prevs ) {
          //println("looking for replaces at " + i + "," + j)
          val (score,ops,_,_) = table(i)(j)
          val s = num.plus(score, weight(Some(this.nodes(this_cur)), Some(that.nodes(that_cur))))
          update_min((s,
                      Operation(this_cur, that_cur,
                                if (id(this.nodes(this_cur)) == id(that.nodes(that_cur))) {
                                  OpEqual
                                } else {
                                  OpReplace
                                }),
                      i, j))
        }

        min.get
      }
    }}

    val pointers = new mutable.ListBuffer[(Int,Int)]
    pointers.append((this.nodes.size - 1, that.nodes.size - 1))
    while ( this.prev_nodes(pointers.head._1).size > 0 || that.prev_nodes(pointers.head._2).size > 0 ) {
      val (_,_,i,j) = table(pointers.head._1)(pointers.head._2)
      pointers.prepend((i,j))
    }
    
    val ops = pointers.map{x => table(x._1)(x._2)._2}.toList

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
        case Operation(_, _, OpNone) => {
          throw new RuntimeException("OpNone")
        }
      }
    }

    // println(this_trans, this.nodes)
    // println(that_trans, that.nodes)

    val n = mutable.ArrayBuffer.fill(count)(this.nodes(0)) //! placeholder として不可能な値を使う
    for ( (i, j) <- this_trans ) {
      n(j) = this.nodes(i)
    }
    for ( (i, j) <- that_trans ) {
      if ( n(j) != this.nodes(0)  ||  i == 0 ) {
        n(j) = merge(n(j), that.nodes(i))
      } else {
        n(j) = that.nodes(i)
      }
    }
    val e = (for ( (i,j) <- this.edges ) yield {
      Pair(this_trans(i), this_trans(j))
    }) ++ (for ( (i,j) <- that.edges ) yield {
      Pair(that_trans(i), that_trans(j))
    }).toSet

    Dag(n.toIndexedSeq, e)
  }

  def trace[S](seq: Seq[S], id: T=>String = x=>x.toString)(implicit id2:S=>String=id): Option[List[Int]] = {
    def _trace(rseq: Seq[Int], acc: List[Int]): Option[List[Int]] =
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
      //! TODO: おなじ文字が2回以上ある時に対応
      if ( seq.filter(x => map.get(id2(x)).isEmpty).size > 0 ) {
        None
      } else {
        val rseq = seq.reverse.map(x => map(id2(x)))
        _trace(rseq.tail, List(rseq.head))
      }
    }
  }

  def dot(nodeformat:(Int,T)=>String = (i,x)=>"N_%d[label=\"%s\"];".format(i, x.toString)): List[String] =
    List("digraph g {",
         "  rankdir = LR;") ++
    (for ((x,i) <- nodes.zipWithIndex) yield {
      " " + nodeformat(i, x)
    }) ++
    edges.map(x => "  N_%d -> N_%d;".format(x._1, x._2)).toList.sorted ++
    List("}")

  def compact(concat: (T,T)=>T): Dag[T] = {
    val visited = new mutable.HashSet[Int]
    def compactable_edges(root: Int, buff: List[Int], acc: Set[List[Int]]): Set[List[Int]] = {
      val prevs = prev_nodes(root)
      if ( buff.size == 0 ) {
        compactable_edges(root, root :: buff, acc)
      } else if ( prevs.size == 0 ) {
        acc + buff
      } else if ( prevs.size == 1 ) {
        val p = prevs.head
        val nx = next_nodes(p)
        if ( nx.size == 1 ) {
          compactable_edges(p,
                            p :: buff,
                            acc)
        } else if ( visited contains p ) {
          acc + buff
        } else {
          visited += p
          compactable_edges(p,
                            List(),
                            acc + buff)
        }
      } else {
        Set(buff) ++ (prevs.foldLeft(Set[List[Int]]())((s,x) =>
          s ++ compactable_edges(x, List(), acc)
          ))
      }
    }

    val compactable = compactable_edges(nodes.size - 1, List(), Set()).toArray.sorted(Ordering.by[List[Int], Int](_.head))
    val itrans = new mutable.HashMap[Int, (Int, T)] ++
                 Map(nodes.zipWithIndex.map(_ match {case (n,i) => (i, (i, n))}): _*)

    for ( ls <- compactable if ls.size >= 2) {
      //System.err.println(ls, ls.map(nodes(_)).reduce(concat))//!

      val h = ls.head
      val ids = ls.foldLeft(Set[Int]())((s,x) => s + x).toArray.sorted
      val n = ids.map(nodes(_)).reduce(concat)
      for ( i <- ids ) {
        itrans(i) = (ids.head, n)
      }
    }
    val itrans_sorted = itrans.toArray.sorted(Ordering.by[(Int, (Int, T)), Int](_._1))
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
    Dag(nn.toIndexedSeq, ee)
  }
}

object Main {
  def main(args: Array[String]) {
    import scala.io
    val strings = args.map(io.Source.fromFile(_).getLines.toList).flatMap(x => x).toList
    val msa = new MultipleSequenceAlignment[Char](strings.map(x => ("^"+x+"$").toCharArray.toIndexedSeq))
    val dag = msa.align.compact((x,y) => x.concat(y))
    def nodeformat(i: Int, x: MultipleSequenceAlignment.Node[Char]): String = {
      def escape(x: String) = x.replace("\\","\\\\").replace("\"", "\\\"")
      "  N_%d[label=\"%s\",fontsize=%f];".format(i, escape(x.label.map(_.toString).mkString),
                                       10 + scala.math.log(x.freq) * 3)
    }
    for ( line <- dag.dot(nodeformat) ) {
      println(line)
    }
  }
}
