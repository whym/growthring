/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

import org.whym.growthring._

import scala.collection.JavaConverters._
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

/**
 * @author Yusuke Matsubara <whym@whym.org>
 */
@RunWith(classOf[JUnitRunner])
class TestMultipleSequenceAlignment extends FunSuite {
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

  def linear_dag(str: String): Dag[Char] = Dag((str).toCharArray.toList,
                                               Range(0, str.length-1).map(x => (x,x+1)).toSet)
  test("linear align: simple replace") {
    expect(Dag(("abBc$").toCharArray.toList,
           Set((0,1), (1,3),
               (0,2), (2,3),
               (3,4)))) {
      linear_dag("abc$").align(linear_dag("aBc$"), weight())
    }
  }

  test("linear align: many vs one replace") {
    expect(Dag("aBzbc$".toCharArray.toList,
               Set((0,1), (1,4),
                   (0,2), (2,3), (3,4),
                   (4,5)))) {
      linear_dag("aBc$").align(linear_dag("azbc$"), weight())
    }
  }

  test("linear align: deletions") {
    expect(Dag("abzzc$".toCharArray.toList,
               Set((0,1), (1,2), (2,3), (3,4),
                   (1,4),
                   (4,5)))) {
      linear_dag("abzzc$").align(linear_dag("abc$"), weight())
    }
  }

  test("linear align: insertion + deletion") {
    expect(Dag("$abcz$".toCharArray.toList,
               Set((0,1), (1,2), (2,3),
                   (0,2), (2,3),
                   (3,4), (3,5),
                   (4,5)
                   ))) {
      linear_dag("$bcz$").align(linear_dag("$abc$"), weight())
    }
  }

  test("dag align: subsumed") {
    val dag1 = Dag("$bB$".toCharArray.toList,
                   Set((0, 1),
                       (0, 2),
                       (1, 3),
                       (2, 3))
                   )
    //! これは複数パスでアラインする実装ができるまでは通らない
    // expect(dag1) {
    //   dag1.align(dag1, weight())
    // }
    expect(dag1) {
      dag1.align(linear_dag("$b$"), weight())
    }
    expect(dag1) {
      dag1.align(linear_dag("$B$"), weight())
    }
  }

  test("dag align: one-node added") {
    val dag1 = Dag("$bB$".toCharArray.toList,
                   Set((0, 1),
                       (0, 2),
                       (1, 3),
                       (2, 3))
                   )
    val dag2 = Dag("$bA$".toCharArray.toList,
                   Set((0, 1),
                       (0, 2),
                       (1, 3),
                       (2, 3))
                   )
    expect(Dag("$bBA$".toCharArray.toList,
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
    expect(Dag("azbzzc$".toCharArray.toList,
               Set((0,1), (1,2), (2,3), (3,4), (4,5),
                   (0,2), (2,5),
                   (5,6)))) {
      linear_dag("abc$").align(linear_dag("azbzzc$"), weight())
    }
  }

  test("linear align: simple insertions") {
    expect(Dag("$zbxx$".toCharArray.toList,
               Set((0,1), (1,2), (2,3), (3,4), (4,5),
                   (0,2), (2,5)))) {
      linear_dag("$b$").align(linear_dag("$zbxx$"), weight())
    }
  }

  test("linear align: JA") {
    expect(Dag("$色いろは匂にえほどへ散とちりぬるを$".toCharArray.toList,
               Set((0,1), (1,4), (4,5), (5,7), (7,9), (9, 11), (11,14), (14,15), (15,16), (16,17), (17,18),
                   (0,2), (2,3), (3,4), (4, 6), (6,8), (8,10), (10,12), (12,13), (13,14)))) {
      linear_dag("$色は匂えど散りぬるを$").align(linear_dag("$いろはにほへとちりぬるを$"), weight())
    }
  }

  test("test msa") {
    val m = new MultipleSequenceAlignment(List("$abc$".toCharArray.toList,
                                               "$cbc$".toCharArray.toList,
                                               "$bcd$".toCharArray.toList))
    println(m.align)
  }
}
