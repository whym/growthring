/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

import org.whym.growthring._

import scala.collection.JavaConverters._
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

/**
 * @author Yusuke Matsubara <whym@whym.org>
 */
@RunWith(classOf[JUnitRunner])
class TestMultipleSequenceAlignment extends FunSuite {
  test("test msa") {
    val m = new MultipleSequenceAlignment(List("abc".toCharArray.toList,
                                               "cbc".toCharArray.toList))
    println(m)
  }
  def weight(_x: Option[Char], _y: Option[Char]) =
    (_x, _y) match {
      case (Some(x),Some(y)) => if (x == y) 0.0 else 2.0
      case (Some(x),_)    => 1.0
      case (_,Some(x))    => 1.0
      case _              => 1.0
    }

  def linear_dag(str: String): Dag[Char] = Dag(str.toCharArray.toList,
                                               Range(0, str.length-1).map(x => (x,x+1)).toList)
  test("test linear align") {
    println(List("*** 2", linear_dag("abc").align(linear_dag("aBc"), weight)).mkString(" "))
    println(List("*** 3", linear_dag("aBc").align(linear_dag("azbc"), weight)).mkString(" "))
    println(List("*** 4", linear_dag("abzzc").align(linear_dag("abc"), weight)).mkString(" "))
    println(List("*** 5", linear_dag("azbzzcd").align(linear_dag("abcd"), weight)).mkString(" "))
    println(List("*** 6", linear_dag("abc").align(linear_dag("azbzzc"), weight)).mkString(" "))
  }
  test("test msa align") {
    val dag1 = Dag("abBc".toCharArray.toList,
                   List((0, 1),
                        (0, 2),
                        (1, 3),
                        (2, 3))
                   )
    //println(List("*** 1", dag1.align(linear_dag("abc"), weight)).mkString(" "))
  }
}

