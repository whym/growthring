/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

import org.whym.growthring._

import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import scala.collection.mutable
import scala.util.matching.Regex

/**
 * @author Yusuke Matsubara <whym@whym.org>
 */
class TestMain extends FunSuite {

  test("anonymize") {
    assertResult(List("a bc __","bc a")) {
      Main.anonymize(
        new WordRepeats().repeats,
        Covering.greedyLengthFreq,
        Seq("a bc aa", "bc a"), 1, '_', 2, "[ \\n]",
        1, 0, -1).toList
    }
  }

  test("find boundaries") {
    assertResult(Array(4, 4, 4, 4, 5, 12, 12, 12, 12, 12, 12, 12, 13, 14, 17, 17, 17, 17)) {
      Main.findBoundaries("abra_cadabra__bra", new Regex("_"))
    }
  }

  test("config default") {
    import com.typesafe.config.ConfigFactory
    val config = ConfigFactory.load.getConfig("org.whym.growthring")
    assertResult("jsuffixarrays") {
      config.getString("repeatsMethod")
    }
  }

}
