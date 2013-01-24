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
    expect(Set(0,1,4,5,6,7,8)) {
      Covering.any(List((0,1), (4,6), (5,8)))
    }
  }

  test("covering greedy") {
    expect(Set(0,1,5,6,7,8)) {
      Covering.greedy(List((0,1), (4,6), (5,8)))
    }
  }

}
