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
class TestMain extends FunSuite {

  test("anonymize") {
    expectResult(List("a bc __","bc a")) {
                        Main.anonymize(new WordRepeats().repeats,
                                       Covering.greedyLengthFreq,
                                       Seq("a bc aa", "bc a"), 1, '_', 2, "[ \\n]",
                                       0, -1).toList
    }
  }
}
