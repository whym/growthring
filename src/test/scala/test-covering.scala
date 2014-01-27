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

  test("overlaps") {
    import Covering.{overlaps, hasOverlap}
    assertResult(true) {
      overlaps((1,2), (2,3))
    }
    assertResult(true) {
      overlaps((1,2), (3,3))
    }
    assertResult(false) {
      overlaps((1,2), (4,4))
    }
    assertResult(false) {
      overlaps((4,4), (0,2))
    }
    assertResult(false) {
      hasOverlap(List((0,1), (3,4), (6,9)))
    }
  }

  test("covering greedy length") {
    assertResult(Set(0,1,5,6,7,8)) {
      Covering.greedyLength("abcdefghi".toCharArray,
                            List((0,1), (3,5), (5,8)))
    }
  }

  test("covering greedy length and size") {
    assertResult(Set(0,1,5,6,7,8)) {
      Covering.greedyLengthFreq("abcdefghi".toCharArray,
                                List((0,1), (3,5), (5,8)))
    }
  }

  test("covering greedy conserv") {
    assertResult(Set(0,1,5,6,7,8,10,11)) {
      Covering.greedyConservative("abcdefghi abcdef".toCharArray,
                                  List((0,1), (3,5), (5,8), (10,11), (13,15)))
    }
  }

  test("covering greedySliced") {
    assertResult(Set(0,1,3,4,6,7,8,9,10)) {
      Covering.greedySliced("abcdefghijk".toCharArray,
                            List((0,1), (3,6), (6,10)))
    }
  }

  test("covering greedySliced 2") {
    assertResult(List(0,1,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18).sorted) {
      Covering.greedySliced("abcdefghijklmnopqrs".toCharArray,
                            List((0,4), (3,10), (10,18))).toList.sorted
    }
  }

  

  test("covering dp") {
    assertResult(List(0,1,2,3,10,11,12,13,14,15,16,17,18)) {
      Covering.dp("abcdefghijklmnopqrs".toCharArray, List((0,3), (3,10), (10,18))).toList.sorted
    }
  }
  test("covering dp 2") {
    assertResult(Set(0,1,5,6,7,8,10,11,13,14,15)) {
      Covering.dp("abcdefghi abcdef".toCharArray,
                  List((0,1), (3,5), (5,8), (10,11), (13,15), (15,16)))
    }
  }

  test("covering exhaustive") {
    assertResult(List(0,1,2,3,10,11,12,13,14,15,16,17,18)) {
      Covering.exhaustive("abcdefghijklmnopqrs".toCharArray, List((0,3), (3,10), (10,18))).toList.sorted
    }
  }

  test("exhaustive1") {
    assertResult(List(1,2,3,6,7,8)) {
      Covering.exhaustive("abcdefghijklmnopqrs".toCharArray, List((1,3), (4,6), (6,8))).toList.sorted
    }
  }

  test("dp1") {
    assertResult(List(1,2,3,6)) {
      Covering.dp("abcdefghijklmnopqrs".toCharArray, List((1,3), (3,4), (6,6))).toList.sorted
    }
  }

  test("exhaustive should be identical to dp") {
    import scala.math
    val str = ('a' until 'z').mkString("") + ('0' until '9').mkString("") + ('A' until 'Z').mkString("")
    val rsize = (math.sqrt(str.size) / 2).toInt

    for ( i <- 1 until 20 ) {
      lazy val regions: Stream[(Int,Int)] = Stream.tabulate(rsize) {
        case 0 => {
          val r = (math.random * rsize).toInt
          (r, r + (math.random * rsize).toInt)
        }
        case n => {
          val r1 = (math.random * rsize).toInt
          val r2 = (math.random * rsize).toInt
          (regions(n-1)._1 + r1 + 1, regions(n-1)._2 + r1 + r2 + 1)
        }
      }
      val reg = regions.toList.sorted.filter(x => x._1 < str.size && x._2 < str.size).toVector.sorted
      System.err.println(reg)
      System.err.println("e " + Covering.exhaustive(str.toCharArray, reg))
      System.err.println("d " + Covering.dp(str.toCharArray, reg))
      assertResult(Covering.exhaustive(str.toCharArray, reg).size) {
        Covering.dp(str.toCharArray, reg).size
      }
    }
  }

}
