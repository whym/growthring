/**
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

import org.whym.growthring._

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import org.scalatest.funsuite.AnyFunSuite

/**
  * @author Yusuke Matsubara <whym@whym.org>
  */
class TestNgramBlame extends AnyFunSuite {

  test("ngram blame concat") {
    assertResult(List(
      (0, 1, 1),
      (1, 5, 0),
      (10, 11, 0))) {
      NgramBlame.concat(List(
        (0, 1, 1),
        (1, 3, 0),
        (3, 4, 0),
        (4, 5, 0),
        (10, 11, 0)))
    }
  }

  test("ngram blame") {
    assertResult(Set((0, 3, 0), (3, 5, 1))) {
      NgramBlame.blameGreedy("abcde", Vector("abcDE", "ABcde"), 3)
    }
  }

  test("ngram blame 2") {
    assertResult(Set((0, 4, 2), (4, 5, 1), (5, 7, 3))) {
      NgramBlame.blameGreedy("abcdefgh", Vector("abcDE", "ABcde", "abcd", "defg"), 3)
    }
  }

  test("ngram blame 3") {
    assertResult(Set((0, 3, 1), (3, 6, 0))) {
      NgramBlame.blameGreedy("aaabbb", Vector("bbbccc", "cccaaa"), 3)
    }
  }

}
