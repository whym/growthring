/**
  * DESCRIBE THIS PROGRAM HERE
  *
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

import org.whym.growthring._

import scala.collection.JavaConverters._
import java.{ io => jio }
import org.scalatest.funsuite.AnyFunSuite

class TestSuffixArrays extends AnyFunSuite {
  import org.whym.growthring.{ SuffixArrays => SA }

  test("load and store") {
    val temp = jio.File.createTempFile("temp", ".dat")

    assertResult(Some(28)) {
      SA.store(Array(255, 2, 33, 4), Array(0, 2, 3, 1), new jio.FileOutputStream(temp))
    }
    assertResult((Array(255, 2, 33, 4).toIndexedSeq, Array(0, 2, 3, 1).toIndexedSeq)) {
      val a = SA.load(new jio.FileInputStream(temp)).get
      (a.arr.toIndexedSeq, a.sa.toIndexedSeq)
    }
  }

  test("stringToUchars") {
    assertResult(IndexedSeq(255, 255)) {
      SA.stringToUchars(new String(Array(65535.asInstanceOf[Char])))
    }
    assertResult(IndexedSeq(3, 0, 255, 255, 5, 0)) {
      SA.stringToUchars(new String(Array(
        3.asInstanceOf[Char],
        65535.asInstanceOf[Char],
        5.asInstanceOf[Char])))
    }
    assertResult("Hello World") {
      SA.ucharsToString(SA.stringToUchars("Hello World").toIndexedSeq)
    }
  }

  test("find") {
    val s = "abracadabra"
    val sa = SA.build(s)
    def rfind: String => Seq[Int] = _.r.findAllMatchIn(s).map(_.start).toSeq
    assertResult(rfind("b")) {
      sa.find(SA.stringToUchars("b")).sorted
    }
    assertResult(rfind("abra")) {
      sa.find(SA.stringToUchars("abra")).sorted
    }
    assertResult(rfind("a")) {
      sa.find(SA.stringToUchars("a")).sorted
    }
  }

  test("bwt") {
    val s = "abracadabra$".toCharArray.map(_.asInstanceOf[Int])
    val sa = SA.build(s, "jsuffixarrays")
    assertResult("ard$rcaaaabb") {
      new String(Range(0, s.size).map(sa.bwt(_).asInstanceOf[Char]).toArray)
    }
  }
  test("internalNodes") {
    val s = "abracadabra".toCharArray.map(_.asInstanceOf[Int])
    val sa = SA.build(s, "jsuffixarrays")
    assertResult(Seq(
      SA.NodePointer(0, 5, 1),
      SA.NodePointer(1, 3, 4))) {
      sa.okanohara(1)
    }
  }
  test("okanohara Repeats") {
    val s = "abracadabra".toCharArray.map(_.asInstanceOf[Int])
    val sa = SA.build(s, "jsuffixarrays")
    assertResult(IndexedSeq(
      Repeat(1, Set(10, 0, 7, 3, 5)),
      Repeat(4, Set(7, 0)))
    ) {
      for (x <- sa.okanohara_repeats(1)) {
        for (y <- x) {
          println(y)
        }
      }
      sa.okanohara_repeats(1)
    }
  }

}
