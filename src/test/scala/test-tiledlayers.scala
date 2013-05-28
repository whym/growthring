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
class TestTiledLayers extends FunSuite {

  test("tiled layers simple") {
    expectResult(List(Set((0,1), (1,4), (5,8)), Set((3,5)))) {
      TiledLayers.greedyTiling("abcdefghi".toCharArray,
                               List((0,1), (1,4), (3,5), (5,8)))
    }
  }
}
