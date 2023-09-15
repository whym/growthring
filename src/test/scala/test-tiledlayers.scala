/**
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

import org.whym.growthring._

import scala.jdk.CollectionConverters._
import org.scalatest.funsuite.AnyFunSuite

/**
  * @author Yusuke Matsubara <whym@whym.org>
  */
class TestTiledLayers extends AnyFunSuite {

  test("tiled layers simple") {
    import TiledLayers.{
      Single => S,
      Begin => B,
      End => E,
      Inside => I,
      Outside => O
    }
    assertResult(List(
      Vector(B(), E(), O(), B(), I(), E(), O(), O(), O()),
      Vector(O(), B(), I(), I(), E(), B(), I(), I(), E()))) {
      TiledLayers.greedyTiling(
        "abcdefghi".toCharArray,
        List((0, 1), (1, 4), (3, 5), (5, 8)))
    }
  }
}
