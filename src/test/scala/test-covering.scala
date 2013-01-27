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
class TestCovering extends FunSuite {

  test("covering any") {
    expectResult(Set(0,1,4,5,6,7,8)) {
      Covering.any("aaaaaaaaa".toCharArray,
                   List((0,1), (4,6), (5,8)))
    }
  }

  test("covering greedy") {
    expectResult(Set(0,1,5,6,7,8)) {
      Covering.greedy("aaaaaaaaa".toCharArray,
                      List((0,1), (3,5), (5,8)))
    }
  }

  test("covering greedySliced") {
    expectResult(Set(0,1,3,4,6,7,8,9,10)) {
      Covering.greedySliced("aaaaaaaaaaa".toCharArray,
                            List((0,1), (3,6), (6,10)))
    }
  }

  test("covering greedySliced 2") {
    expectResult(List(0,1,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18).sorted) {
      Covering.greedySliced("aaaaaaaaaaaaaaaaaaa".toCharArray,
                            List((0,4), (3,10), (10,18))).toList.sorted
    }
  }

}
