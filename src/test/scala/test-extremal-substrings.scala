/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

import org.whym.growthring._

import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import scala.collection.mutable

object TestExtremalSubstrings {
  case class Substring(parent: String, start: Int, end: Int, slice: String) {
    def length = end - start
    def head_removed  = Substring(parent, start+1, end, parent.slice(start+1, end))
    def last_removed  = Substring(parent, start, end-1, parent.slice(start, end-1))
    override def equals(x: Any) = x.isInstanceOf[Substring] && slice == x.asInstanceOf[Substring].slice
    override def hashCode = slice.hashCode
  }
}

/**
 * @author Yusuke Matsubara <whym@whym.org>
 */
class TestExtremalSubstrings extends FunSuite {
  import TestExtremalSubstrings._

  def substrings(str: String): Seq[Substring] =
    for ( i <- 0 to str.length;
          j <- (i+1) to str.length ) yield {
            Substring(str, i, j, str.slice(i,j))
         }

  def count[T](seq: Seq[T]): Map[T,List[T]] = {
    val counts = new mutable.HashMap[T,List[T]] {
      override def default(x:T) = List[T]()
    }
    for ( s <- seq ) {
      counts(s) = s :: counts(s)
    }
    return counts.toMap
  }

  // def minimals(set: Set[Substring]): Set[Substring] =
  //   set.filter(str => {
  //     ( str.length <= 1 || !(set contains str.head_removed) ) &&
  //     ( str.length <= 1 || !(set contains str.last_removed) )
  //   })

  def minimals(set: Set[(Int,Int)]): Set[(Int,Int)] =
    set.filter(self => {
      !(set contains Pair(self._1+1, self._2)) &&
      !(set contains Pair(self._1,   self._2-1))
    })

  def maximals(set: Set[(Int,Int)]): Set[(Int,Int)] =
    set.filter(self => {
      !(set contains Pair(self._1-1, self._2)) &&
      !(set contains Pair(self._1,   self._2+1))
    })

  def myMinUniques(str: String, threshold: Int=1) = {
    val counts = count(substrings(str)).filter(x => x._2.size <= threshold)
    //minimals(counts.keySet).map(counts).reduce(_++_).map(x => (x.start, x.end - 1)).toList.sorted
    minimals(counts.values.reduce(_++_).map(x => (x.start, x.end - 1)).toSet).toList.sorted
  }

  def myMaxRepeats(str: String, threshold: Int=2) = {
    val counts = count(substrings(str)).filter(x => x._2.size >= threshold)
    maximals(counts.values.reduce(_++_).map(x => (x.start, x.end - 1)).toSet).toList.sorted
  }

  import org.whym.growthring.{ExtremalSubstrings => ES}

  test("string to unsigned: abc") {
    expect(List(0x61,0,0x62,0,0x63,0)) {
      ES.stringToUnsigneds("abc").toList
    }
  }

  test("uniques: abaababa") {
    expect(myMinUniques("abaababa")){
      new ES("abaababa").minUniques
    }
  }

  test("repeats(2): abaababa") {
    expect(Seq((0,2), (3,5), (5,7))){
      new ES("abaababa").maxRepeats
    }
    expect(Seq((0,2), (3,5), (5,7))){
      new ES("abaababa").maxRepeats2
    }
  }

  test("uniques: banana") {
    expect(Seq((0,0), (2,4))){
      new ES("banana").minUniques
    }
  }

  test("repeats(2): banana") {
    expect(myMaxRepeats("banana")) {
      new ES("banana").maxRepeats
    }
  }

  test("repeats(2): アブラカダブラ") {
    expect(myMaxRepeats("アブラカダブラ")) {
      new ES("アブラカダブラ").maxRepeats
    }
  }

  // test("repeats(2): abradadabrara") {
  //   expect(Seq((1,2), (5,6))){
  //     new ES("abracadabrara").maxRepeats
  //   }
  // }

  test("repeats(3): aaa") {
    expect(Seq((0,0), (1,1), (2,2))){
      new ES("aaa").maxRepeats(3)
    }
  }

  test("repeats(3): banana") {
    expect(Seq((1,1), (3,3), (5,5))){
      new ES("banana").maxRepeats(3)
    }
  }

  test("repeats(3): bananan") {
    expect(Seq((1,2), (3,4), (5,6))){
      new ES("bananan").maxRepeats(3)
    }
  }

  test("repeats(4): bananan") {
    expect(Seq()){
      new ES("bananan").maxRepeats(4)
    }
  }

  test("repeats(4): bananan ban") {
    expect(myMaxRepeats("bananan ban", 4)){
      new ES("bananan ban").maxRepeats(4)
    }
  }

  test("repeats(5): ACTATGAAGACAGGATCGATGCTA...") {
    expect(myMaxRepeats("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA", 4)){
      new ES("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA").maxRepeats(4)
    }
  }

  test("uniques(2): banana") {
    expect(myMinUniques("banana", 2)){
      new ES("banana").minUniques2
    }
  }

  test("uniques(2): bananana") {
    expect(myMinUniques("bananana", 2)){
      new ES("bananana").minUniques2
    }
  }

  test("uniques(2): ACTATGAAGACAGGATCGATGCTA...") {
    expect(myMinUniques("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA", 2)){
      new ES("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA").minUniques2
    }
  }

  test("de Bruijn repeats(2): aaaabaabbababbbbaaa") {
    expect((0 to 16).toList.map(x => (x, x+2))) {
      new ES("aaaabaabbababbbbaaa").maxRepeats(2)
    }
  }

  test("de Bruijn uniques(2): aaaabaabbababbbbaaa") {
    expect((0 to 15).toList.map(x => (x, x+3))) {
      new ES("aaaabaabbababbbbaaa").minUniques
    }
  }

}
