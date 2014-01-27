/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

import org.whym.growthring._

import scala.collection.JavaConverters._
import org.scalatest.FunSuite

/**
 * @author Yusuke Matsubara <whym@whym.org>
 */
class TestMultipleSequenceAlignment extends FunSuite {
  import org.whym.growthring.{MultipleSequenceAlignment => MSA}

  def weight()(implicit
               eql:Double=0.0,
               del:Double=1.0,
               ins:Double=1.0,
               rep:Double=1.5,
               ooo:Double=10.0) =
    (_x: Option[Char], _y: Option[Char]) => {
    (_x, _y) match {
      case (Some(x),Some(y)) => if (x == y) eql else rep
      case (Some(x),_)    => del
      case (_,Some(x))    => ins
      case _              => ooo
    }
    }

  def linear_dag(str: String): Dag[Char] = Dag((str).toIndexedSeq,
                                               Range(0, str.length-1).map(x => (x,x+1)).toSet)
  test("linear align: simple replace") {
    assertResult(Dag(("abBc$").toIndexedSeq,
           Set((0,1), (1,3),
               (0,2), (2,3),
               (3,4)))) {
      linear_dag("abc$").align(linear_dag("aBc$"), weight())
    }
  }

  test("linear align: many vs one replace") {
    assertResult(Dag("aBzbc$".toIndexedSeq,
               Set((0,1), (1,4),
                   (0,2), (2,3), (3,4),
                   (4,5)))) {
      linear_dag("aBc$").align(linear_dag("azbc$"), weight())
    }
  }

  test("linear align: deletions") {
    assertResult(Dag("abzzc$".toIndexedSeq,
               Set((0,1), (1,2), (2,3), (3,4),
                   (1,4),
                   (4,5)))) {
      linear_dag("abzzc$").align(linear_dag("abc$"), weight())
    }
  }

  test("linear align: insertion + deletion") {
    assertResult(Dag("$abcz$".toIndexedSeq,
               Set((0,1), (1,2), (2,3),
                   (0,2), (2,3),
                   (3,4), (3,5),
                   (4,5)
                   ))) {
      linear_dag("$bcz$").align(linear_dag("$abc$"), weight())
    }
  }

  test("dag align: subsumed") {
    val dag1 = Dag("$bB$".toIndexedSeq,
                   Set((0, 1),
                       (0, 2),
                       (1, 3),
                       (2, 3))
                   )
    //! これは複数パスでアラインする実装ができるまでは通らない
    // assertResult(dag1) {
    //   dag1.align(dag1, weight())
    // }
    assertResult(dag1) {
      dag1.align(linear_dag("$b$"), weight())
    }
    assertResult(dag1) {
      dag1.align(linear_dag("$B$"), weight())
    }
  }

  test("dag align: one-node added") {
    val dag1 = Dag("$bB$".toIndexedSeq,
                   Set((0, 1),
                       (0, 2),
                       (1, 3),
                       (2, 3))
                   )
    val dag2 = Dag("$bA$".toIndexedSeq,
                   Set((0, 1),
                       (0, 2),
                       (1, 3),
                       (2, 3))
                   )
    assertResult(Dag("$bBA$".toIndexedSeq,
               Set((0, 1),
                   (0, 2),
                   (0, 3),
                   (1, 4),
                   (2, 4),
                   (3, 4))
             )) {
      dag1.align(dag2, weight())
    }
  }

  test("linear align: insertions") {
    assertResult(Dag("azbzzc$".toIndexedSeq,
               Set((0,1), (1,2), (2,3), (3,4), (4,5),
                   (0,2), (2,5),
                   (5,6)))) {
      linear_dag("abc$").align(linear_dag("azbzzc$"), weight())
    }
  }

  test("linear align: simple insertions") {
    assertResult(Dag("$zbxx$".toIndexedSeq,
               Set((0,1), (1,2), (2,3), (3,4), (4,5),
                   (0,2), (2,5)))) {
      linear_dag("$b$").align(linear_dag("$zbxx$"), weight())
    }
  }

  test("linear align: recurring letters") {
    assertResult(Dag("^baba$".toIndexedSeq,
               Set((0,1), (1,2), (2,3), (3,5),
                   (0,2), (3,4), (4,5)))) {
      linear_dag("^aba$").align(linear_dag("^bab$"), weight())
    }
  }

  test("linear align: JA") {
    assertResult(Dag("$色いろは匂にえほどへ散とちりぬるを$".toIndexedSeq,
               Set((0,1), (1,4), (4,5), (5,7), (7,9), (9, 11), (11,14), (14,15), (15,16), (16,17), (17,18),
                   (0,2), (2,3), (3,4), (4, 6), (6,8), (8,10), (10,12), (12,13), (13,14)))) {
      linear_dag("$色は匂えど散りぬるを$").align(linear_dag("$いろはにほへとちりぬるを$"), weight())
    }
  }

  test("test trace: 1") {
    assertResult(Some(List(2))) {
      linear_dag("$b$").align(linear_dag("$zbxx$"), weight()).trace("b".toIndexedSeq)
    }
  }
  test("test trace: fail") {
    assertResult(None) {
      linear_dag("$b$").align(linear_dag("$zbxx$"), weight()).trace("o".toIndexedSeq)
    }
  }
  test("test trace: same letters") {
    assertResult(Some(List(4,5,6,7))) {
      linear_dag("$aaaxxbbb$").trace("xxbb".toIndexedSeq)
    }
  }

  test("test dot") {
    assertResult(List("digraph g {",
                "  rankdir = LR;",
                "  N_0[label=\"^\"];",
                "  N_1[label=\"a\"];",
                "  N_2[label=\"b\"];",
                "  N_3[label=\"c\"];",
                "  N_4[label=\"A\"];",
                "  N_5[label=\"$\"];",
                "  N_0 -> N_1;",
                "  N_0 -> N_4;",
                "  N_1 -> N_2;",
                "  N_2 -> N_3;",
                "  N_3 -> N_5;",
                "  N_4 -> N_5;",
                "}").map(_.trim)) {
      Dag("^abcA$".toIndexedSeq,
          Set((0,1), (1,2), (2,3), (3,5),
              (0,4), (4,5))).dot().map(_.trim)
    }
  }

  test("test compact: one path") {
    assertResult(Dag(Vector("^", "abc", "A", "$"),
               Set((0,1), (1,3),
                   (0,2), (2,3)))) {
      Dag("^abcA$".toIndexedSeq.map({x: Char => x.toString}),
          Set((0,1), (1,2), (2,3), (3,5),
              (0,4), (4,5))).compact((x,y) => x+y)
    }
  }

  test("test compact: twin") {
    assertResult(Dag(Vector("^", "ab", "AB", "$"),
               Set((0,1), (1,3),
                   (0,2), (2,3)))) {
      Dag("^abAB$".toIndexedSeq.map({x: Char => x.toString}),
          Set((0,1), (1,2), (2,5),
              (0,3), (3,4), (4,5))).compact((x,y) => x+y)
    }
  }

  test("test compact: multiple") {
    assertResult(Dag(Vector("^", "ab", "AB", "CDE","c", "$"),
               Set((0,1), (1,3), (3,5),
                   (2,5),
                   (0,4), (4,5),
                   (0,2), (2,3)))) {
      Dag("^aAbBCDEc$".toIndexedSeq.map({x: Char => x.toString}),
          Set((0,2), (2,4), (4,5), (5,6), (6,7), (7,9),
              (4,9),
              (0,8), (8,9),
              (0,1), (1,3), (3,5))).compact((x,y) => x+y)
    }
  }

  test("test compact: shortcut") {
    assertResult(Dag(Vector("^", "1", "23","45", "$"),
               Set((0,1), (1,4), (0,2), (2,3), (0,3), (3,4)))) {
      Dag("^12345$".toIndexedSeq.map({x: Char => x.toString}),
          Set((0,1), (1,6),
              (0,2), (2,3), (3,4),
              (0,4), (4,5), (5,6)
            )).compact((x,y) => x+y)
    }
  }

  test("test compact: start") {
    assertResult(Dag(Vector("^ab", "CDE","c", "$"),
               Set((0,1), (1,3),
                   (0,2), (2,3)))) {
      Dag("^abCDEc$".toIndexedSeq.map({x: Char => x.toString}),
          Set((0,1), (1,2), (2, 6), (6,7),
              (2,3), (3,4), (4,5), (5,7))).compact((x,y) => x+y)
    }
  }

  test("test node concat") {
    assertResult(MSA.Node("abcdef".toIndexedSeq, 0, 4)) {
      MSA.Node("abcdef".toIndexedSeq, 0, 2) concat MSA.Node("cdef".toIndexedSeq, 0, 2)
    }
  }

  test("test msa: three letters") {
    val m = new MSA(List("^abc$".toIndexedSeq,
                         "^xbc$".toIndexedSeq,
                         "^bcd$".toIndexedSeq))
    val align = m.align
    assertResult(Some(List(0,1,3,4,6))) {
      align.trace("^abc$".toIndexedSeq,
                    x => x.label.head.toString)(_.toString)
    }
    assertResult(Some(List(0,2,3,4,6))) {
      align.trace("^xbc$".toIndexedSeq,
                    x => x.label.head.toString)(_.toString)
    }
    assertResult(Some(List(0,3,4,5,6))) {
      align.trace("^bcd$".toIndexedSeq,
                    x => x.label.head.toString)(_.toString)
    }
    assertResult(Some(List(0,3,4,5,6))) {
      align.trace("^bcd$".toIndexedSeq,
                    x => x.label.head.toString)(_.toString)
    }
  }

  test("test msa: empty sequence") {
    val m = new MSA(List("^$".toIndexedSeq,
                         "^ac$".toIndexedSeq,
                         "^xb$".toIndexedSeq))
    assertResult(Some(List(0,2,4,5))) {
      m.align.trace("^xb$".toIndexedSeq,
                    x => x.label.head.toString)(_.toString)
    }
    assertResult(Some(List(0,1,3,5))) {
      m.align.trace("^ac$".toIndexedSeq,
                    x => x.label.head.toString)(_.toString)
    }
  }
}
