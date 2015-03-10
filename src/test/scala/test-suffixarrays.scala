/**
 * DESCRIBE THIS PROGRAM HERE
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

import org.whym.growthring._

import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import java.{io => jio}

class TestSuffixArrays extends FunSuite {
  import org.whym.growthring.{SuffixArrays => SA}

  test("load and store") {
    val temp = jio.File.createTempFile("temp",".dat")
    
    assertResult(Some(28)) {
      SA.store(Array(255,2,33,4), Array(0,2,3,1), new jio.FileOutputStream(temp))
    }
    assertResult((Array(255,2,33,4).deep, Array(0,2,3,1).deep)) {
      val a = SA.load(new jio.FileInputStream(temp)).get
      (a.arr, a.sa)
    }
  }

  test("find") {
    val sa = SA.build("abracadabra")
    assertResult(Seq(1,8)) {
      sa.find(SA.stringToUchars("b")).sorted
    }
    assertResult(Seq(0,7)) {
      sa.find(SA.stringToUchars("abra")).sorted
    }
    assertResult(Seq(0,3,5,7,10)) {
      sa.find(SA.stringToUchars("a")).sorted
    }
  }

}

