/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

import org.whym.growthring._

import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import scala.collection.mutable

/**
 * @author Yusuke Matsubara <whym@whym.org>
 */
class TestNgramBlame extends FunSuite {

  test("ngram blame concat") {
    expectResult(List((0,1,1),
                      (1,5,0),
                      (10,11,0)))
    {
      NgramBlame.concat(List((0,1,1),
                             (1,3,0),
                             (3,4,0),
                             (4,5,0),
                             (10,11,0)))
    }
  }

  test("ngram blame") {
    expectResult(Set((0,3,0), (3,5,1))) {
      NgramBlame.blameGreedy("abcde", Vector("abcDE", "ABcde"), 3)
    }
  }

  test("ngram blame 2") {
    expectResult(Set((0,4,2), (4,5,1), (5,7,3))) {
      NgramBlame.blameGreedy("abcdefgh", Vector("abcDE", "ABcde", "abcd", "defg"), 3)
    }
  }

  test("ngram blame 3") {
    expectResult(Set((0,3,1), (3, 6, 0))) {
      NgramBlame.blameGreedy("aaabbb", Vector("bbbccc", "cccaaa"), 3)
    }
  }

}
