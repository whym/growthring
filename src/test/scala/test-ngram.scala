/**
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

import org.whym.growthring._

import scala.collection.JavaConverters._
import scala.collection.mutable
import org.scalatest.funsuite.AnyFunSuite

/**
  * @author Yusuke Matsubara <whym@whym.org>
  */
class TestNgramRepeats extends AnyFunSuite {

  test("ngram queue") {
    assertResult(List(
      List("abc", "d"),
      List("abc", "d", "e"),
      List("d", "e"),
      List("abc", "d", "e", "f"),
      List("d", "e", "f"),
      List("e", "f"),
      List("d", "e", "f", "ghi"),
      List("e", "f", "ghi"),
      List("f", "ghi"))) {
      import org.whym.growthring.{ NgramQueue => NQ }
      val ng = new NQ[String](2, 4)
      (for (s <- Seq("abc", "d", "e", "f", "ghi")) yield {
        ng enqueue s
        ng.getNgrams()
      }).flatten
    }
  }

  test("ngram repeats") {
    assertResult(List(
      (0, 0), (0, 1), (0, 2),
      (1, 1), (1, 2), (1, 3),
      (2, 2), (2, 3),
      (3, 3),
      (5, 5),
      (7, 7), (7, 8), (7, 9), (8, 8), (8, 9),
      (8, 10), (9, 9), (9, 10), (10, 10))) {
      import org.whym.growthring.{ NgramRepeats => NR }
      new NR(3).repeats("abracadabra", 2).sorted
    }
  }

  test("ngram covering") {
    assertResult(List(0, 1, 2, 5, 7, 8, 9)) {
      import org.whym.growthring.{ NgramRepeats => NR }
      val r = new NR(3).repeats("abracadabra", 2)
      Covering.greedyLength("abracadabra".toCharArray, r).toList.sorted
    }
  }
}
