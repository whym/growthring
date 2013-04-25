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
class TestWordRepeats extends FunSuite {

  test("word repeats") {
    expectResult(List((0, 3),
                      (11,14))) {
      import org.whym.growthring.{WordRepeats => WR}
      new WR().repeats("abra  cad  abra", 2).sorted
    }
  }

  test("word repeats with various spaces") {
    expectResult(List((0, 3),
                      (11,14))) {
      import org.whym.growthring.{WordRepeats => WR}
      new WR().repeats("abra\n cad  abra\taa　\nab", 2).sorted
    }
  }

  test("word covering") {
    expectResult(List(0, 1, 2, 3, 9, 10, 11, 12)) {
      import org.whym.growthring.{WordRepeats => WR}
      val r = new WR().repeats("abra cad abra", 2)
      Covering.greedy("abra cad abra".toCharArray, r).toList.sorted
    }
  }
}
