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
class TestExtremalSubstrings extends FunSuite {
  import org.whym.growthring.{ExtremalSubstrings => ES}

  test("string to unsigne: abc") {
    expect(List(0x61,0,0x62,0,0x63,0)) {
      ES.stringToUnsigneds("abc").toList
    }
  }

  test("uniques: abaababa") {
    expect(Seq((2,3), (4,6))){
      new ES("abaababa").minUniques
    }
  }

  test("repeats: abaababa") {
    expect(Seq((0,2), (3,5), (5,7))){
      new ES("abaababa").maxRepeats
    }
  }

  test("uniques: banana") {
    expect(Seq((0,0), (2,4))){
      new ES("banana").minUniques
    }
  }

  test("repeats: banana") {
    expect(Seq((1,3), (3,5))){
      new ES("banana").maxRepeats
    }
  }

  test("repeats: アブラカダブラ") {
    expect(Seq((1,2), (5,6))){
      new ES("アブラカダブラ").maxRepeats
    }
  }

  test("repeats(3): aaa") {
    expect(Seq((0,0), (1,1), (2,2))){
      new ES("aaa").maxRepeats3
    }
  }

  test("repeats(3): banana") {
    expect(Seq((1,1), (3,3), (5,5))){
      new ES("banana").maxRepeats3
    }
  }

  test("repeats(3): bananan") {
    expect(Seq((1,2), (3,4), (5,6))){
      new ES("bananan").maxRepeats3
    }
  }

  test("uniques(2): banana") {
    expect(Seq((0,0), (2,2), (4,4))){
      new ES("banana").minUniques2
    }
  }

  test("uniques(2): banananana") {
    expect(Seq((0,0), (2,4), (4,6))){
      new ES("bananana").minUniques2
    }
  }

}
