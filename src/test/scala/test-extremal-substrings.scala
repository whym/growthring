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
class TestExtremalSubstrings extends FunSuite {
  import org.whym.growthring.{NaiveExtremalSubstrings => NES}

  import org.whym.growthring.{ExtremalSubstrings=>ES, SuffixArrays}

  def newES(s: String) = new ExtremalSubstrings(SuffixArrays.buildJsuffixarrays(s))

  test("round min") {
    assertResult((3,3)) {
      ES.roundMin((5,7))
    }
    assertResult((5,5)) {
      ES.roundMin((10,12))
    }
  }

  test("sliding minimum") {
    assertResult(IndexedSeq(1,1,1,1,2,2)) {
      ES.slidingMinimums(3, Array(1,4,2,1,9,3,2,7))
    }
  }

  test("string to uchar: abc") {
    assertResult(List(0x61,0,0x62,0,0x63,0)) {
      SuffixArrays.stringToUchars("abc").toList
    }
  }

  test("uniques: abaababa") {
    assertResult(NES.minUniques("abaababa")){
      newES("abaababa").minUniques
    }
  }

  test("repeats(2): abaababa") {
    assertResult(Seq((0,2), (3,5), (5,7))){
      newES("abaababa").maxRepeats
    }
    assertResult(Seq((0,2), (3,5), (5,7))){
      newES("abaababa").maxRepeats2
    }
  }

  test("uniques: banana") {
    assertResult(Seq((0,0), (2,4))){
      newES("banana").minUniques
    }
  }

  test("repeats(2): banana") {
    assertResult(NES.maxRepeats("banana")) {
      newES("banana").maxRepeats
    }
  }

  test("repeats(2): アブラカダブラ") {
    assertResult(NES.maxRepeats("アブラカダブラ")) {
      newES("アブラカダブラ").maxRepeats
    }
  }

  // test("repeats(2): abradadabrara") {
  //   assertResult(Seq((1,2), (5,6))){
  //     newES("abracadabrara").maxRepeats
  //   }
  // }

  test("repeats(3): aaa") {
    assertResult(Seq((0,0), (1,1), (2,2))){
      newES("aaa").maxRepeats(3)
    }
  }

  test("repeats(3): banana") {
    assertResult(Seq((1,1), (3,3), (5,5))){
      newES("banana").maxRepeats(3)
    }
  }

  test("repeats(3): bananan") {
    assertResult(Seq((1,2), (3,4), (5,6))){
      newES("bananan").maxRepeats(3)
    }
  }

  test("repeats(4): bananan") {
    assertResult(Seq()){
      newES("bananan").maxRepeats(4)
    }
  }

  test("repeats(4): bananan ban") {
    assertResult(NES.maxRepeats("bananan ban", 4)){
      newES("bananan ban").maxRepeats(4)
    }
  }

  test("repeats(4): ACTATGAAGACAGGATCGATGCTA...") {
    assertResult(NES.maxRepeats("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA", 4)){
      newES("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA").maxRepeats(4)
    }
  }

  test("repeats(4): 日本国民は...") {
    assertResult(NES.maxRepeats("日本国民は、正当に選挙された国会における代表者を通じて行動し、われらとわれらの子孫のために、諸国民との協和による成果と、わが国全土にわたって自由のもたらす恵沢を確保し、政府の行為によって再び戦争の惨禍が起ることのないやうにすることを決意し、ここに主権が国民に存することを宣言し、この憲法を確定する。そもそも国政は、国民の厳粛な信託によるものであって、その権威は国民に由来し、その権力は国民の代表者がこれを行使し、その福利は国民がこれを享受する。これは人類普遍の原理であり、この憲法は、かかる原理に基くものである。われらは、これに反する一切の憲法、法令及び詔勅を排除する。", 4)){
      newES("日本国民は、正当に選挙された国会における代表者を通じて行動し、われらとわれらの子孫のために、諸国民との協和による成果と、わが国全土にわたって自由のもたらす恵沢を確保し、政府の行為によって再び戦争の惨禍が起ることのないやうにすることを決意し、ここに主権が国民に存することを宣言し、この憲法を確定する。そもそも国政は、国民の厳粛な信託によるものであって、その権威は国民に由来し、その権力は国民の代表者がこれを行使し、その福利は国民がこれを享受する。これは人類普遍の原理であり、この憲法は、かかる原理に基くものである。われらは、これに反する一切の憲法、法令及び詔勅を排除する。").maxRepeats(4)
    }
  }

  test("uniques(2): banana") {
    assertResult(NES.minUniques("banana", 2)){
      newES("banana").minUniques2
    }
  }

  test("uniques(2): bananana") {
    assertResult(NES.minUniques("bananana", 2)){
      newES("bananana").minUniques2
    }
  }

  test("uniques(2): ACTATGAAGACAGGATCGATGCTA...") {
    assertResult(NES.minUniques("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA", 2)){
      newES("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA").minUniques2
    }
  }

  test("de Bruijn repeats(2): aaaabaabbababbbbaaa") {
    assertResult((0 to 16).toList.map(x => (x, x+2))) {
      newES("aaaabaabbababbbbaaa").maxRepeats(2)
    }
  }

  test("de Bruijn uniques(2): aaaabaabbababbbbaaa") {
    assertResult((0 to 15).toList.map(x => (x, x+3))) {
      newES("aaaabaabbababbbbaaa").minUniques
    }
  }

}
