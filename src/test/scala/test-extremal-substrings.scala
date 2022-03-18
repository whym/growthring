/**
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

import org.whym.growthring._

import scala.collection.JavaConverters._
import scala.collection.mutable
import org.scalatest.funsuite.AnyFunSuite

/**
  * @author Yusuke Matsubara <whym@whym.org>
  */
class TestExtremalSubstrings extends AnyFunSuite {
  import org.whym.growthring.{ NaiveExtremalSubstrings => NES }

  import org.whym.growthring.{ ExtremalSubstrings => ES, SuffixArrays }

  def newES(s: String) = new ExtremalSubstrings(SuffixArrays.build(s, "jsuffixarrays"))

  test("subsume longer") {
    assertResult(Seq((0, 3))) {
      ES.subsumeLonger(Seq((0, 3), (1, 3)))
    }
  }

  test("subsume shorter") {
    assertResult(Seq((1, 3))) {
      ES.subsumeShorter(Seq((0, 3), (1, 3)))
    }
  }

  test("round max") {
    assertResult((2, 3)) {
      ES.roundMax((5, 7))
    }
    assertResult((5, 6)) {
      ES.roundMax((10, 12))
    }
    assertResult((3, 5)) {
      ES.roundMax((7, 10))
    }
  }

  test("round min") {
    assertResult((3, 3)) {
      ES.roundMin((5, 7))
    }
    assertResult((5, 5)) {
      ES.roundMin((10, 12))
    }
    assertResult((4, 4)) {
      ES.roundMin((7, 10))
    }
  }

  test("sliding minimum") {
    assertResult(Array(1, 1, 1, 1, 2, 2)) {
      ES.slidingMinimums(3, Array(1, 4, 2, 1, 9, 3, 2, 7))
    }
  }

  test("string to uchar: abc") {
    assertResult(List(0x61, 0, 0x62, 0, 0x63, 0)) {
      SuffixArrays.stringToUchars("abc").toList
    }
  }

  test("crosscheck: uniques: abaababa") {
    assertResult(NES.minUniques("abaababa")) {
      newES("abaababa").minUniques()
    }
  }

  test("repeats(2): abaababa") {
    assertResult(Seq((0, 2), (3, 5), (5, 7))) {
      newES("abaababa").maxRepeats()
    }
  }

  test("repeats: banana") {
    assertResult(Seq((1, 3), (3, 5))) {
      newES("banana").maxRepeats()
    }
  }

  test("uniques: banana") {
    assertResult(Seq((0, 0), (2, 4))) {
      newES("banana").minUniques()
    }
  }

  test("crosscheck: repeats(2): banana") {
    assertResult(NES.maxRepeats("banana")) {
      newES("banana").maxRepeats()
    }
  }

  test("crosscheck: repeats(2): アブラカダブラ") {
    assertResult(NES.maxRepeats("アブラカダブラ")) {
      newES("アブラカダブラ").maxRepeats()
    }
  }

  // test("repeats(2): abradadabrara") {
  //   assertResult(Seq((1,2), (5,6))){
  //     newES("abracadabrara").maxRepeats
  //   }
  // }

  test("repeats(3): aaa") {
    assertResult(Seq((0, 0), (1, 1), (2, 2))) {
      newES("aaa").maxRepeats(3)
    }
  }

  test("repeats(3): banana") {
    assertResult(Seq((1, 1), (3, 3), (5, 5))) {
      newES("banana").maxRepeats(3)
    }
  }

  test("repeats(3): bananan") {
    assertResult(Seq((1, 2), (3, 4), (5, 6))) {
      newES("bananan").maxRepeats(3)
    }
  }

  test("repeats(4): bananan") {
    assertResult(Seq()) {
      newES("bananan").maxRepeats(4)
    }
  }

  test("crosscheck: repeats(4): bananan ban") {
    assertResult(NES.maxRepeats("bananan ban", 4)) {
      newES("bananan ban").maxRepeats(4)
    }
  }

  test("repeats(4): ACTATGAAGACAGGATCGATGCTA...") {
    assertResult(NES.maxRepeats("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA", 4)) {
      newES("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA").maxRepeats(4)
    }
  }

  test("repeats(4): 日本国民は...") {
    assertResult(NES.maxRepeats("日本国民は、正当に選挙された国会における代表者を通じて行動し、われらとわれらの子孫のために、諸国民との協和による成果と、わが国全土にわたって自由のもたらす恵沢を確保し、政府の行為によって再び戦争の惨禍が起ることのないやうにすることを決意し、ここに主権が国民に存することを宣言し、この憲法を確定する。そもそも国政は、国民の厳粛な信託によるものであって、その権威は国民に由来し、その権力は国民の代表者がこれを行使し、その福利は国民がこれを享受する。これは人類普遍の原理であり、この憲法は、かかる原理に基くものである。われらは、これに反する一切の憲法、法令及び詔勅を排除する。", 4)) {
      newES("日本国民は、正当に選挙された国会における代表者を通じて行動し、われらとわれらの子孫のために、諸国民との協和による成果と、わが国全土にわたって自由のもたらす恵沢を確保し、政府の行為によって再び戦争の惨禍が起ることのないやうにすることを決意し、ここに主権が国民に存することを宣言し、この憲法を確定する。そもそも国政は、国民の厳粛な信託によるものであって、その権威は国民に由来し、その権力は国民の代表者がこれを行使し、その福利は国民がこれを享受する。これは人類普遍の原理であり、この憲法は、かかる原理に基くものである。われらは、これに反する一切の憲法、法令及び詔勅を排除する。").maxRepeats(4)
    }
  }

  test("crosscheck: uniques(2): banana") {
    assertResult(NES.minUniques("banana", 2)) {
      newES("banana").minUniques()
    }
  }

  test("crosscheck: uniques(2): bananana") {
    assertResult(NES.minUniques("bananana", 2)) {
      newES("bananana").minUniques()
    }
  }

  test("crosscheck: uniques(2): ACTATGAAGACAGGATCGATGCTA...") {
    assertResult(NES.minUniques("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA", 2)) {
      newES("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA").minUniques()
    }
  }

  test("de Bruijn repeats(2): aaaabaabbababbbbaaa") {
    assertResult((0 to 16).toList.map(x => (x, x + 2))) {
      newES("aaaabaabbababbbbaaa").maxRepeats(2)
    }
  }

  test("de Bruijn uniques(2): aaaabaabbababbbbaaa") {
    assertResult((0 to 15).toList.map(x => (x, x + 3))) {
      newES("aaaabaabbababbbbaaa").minUniques()
    }
  }

  test("with boundary (no boundary)") {
    assertResult(Seq((0, 3), (5, 5), (7, 10))) {
      newES("abracadabra").maxRepeats(2, { x => 11 })
    }
  }

  test("with boundary (boundary at 4)") {
    assertResult(Seq((0, 3), (5, 5), (7, 10))) {
      newES("abracadabra").maxRepeats(2, { x => if (x < 4) { 4 } else { 11 } })
    }
  }

  test("with boundary (boundary at 3)") {
    assertResult(Seq((0, 2), (3, 3), (5, 5), (7, 10))) {
      newES("abracadabra").maxRepeats(2, { x => if (x < 3) { 3 } else { 11 } })
    }
  }

  test("with boundary (boundary at 2)") {
    assertResult(Seq((0, 1), (2, 3), (5, 5), (7, 10))) {
      newES("abracadabra").maxRepeats(2, { x => if (x < 2) { 2 } else { 11 } })
    }
  }

  test("with boundary (boundary at 3, 6)") {
    assertResult(Seq((0, 3), (5, 5), (7, 10))) {
      newES("abracadabra").maxRepeats(2, { x =>
        if (x < 4) { 4 }
        else if (x < 7) { 7 }
        else { 11 }
      })
    }
  }
  test("with boundary (3 boundaries)") {
    assertResult(Seq((0, 3), (4, 6), (7, 8), (9, 10), (12, 18))) {
      newES("abracadabra\ncadabra").maxRepeats(2, { x =>
        if (x < 7) { 7 }
        else if (x < 9) { 9 }
        else if (x < 11) { 11 }
        else { 19 }
      })
    }
  }

  test("uniques with no boundary") {
    assertResult(Seq((0, 1), (6, 8), (13, 14))) {
      newES("abrada brada ba").minUniques(2, { x => 15 })
    }
  }

  test("uniques with boundary") {
    assertResult(Seq((0, 1), (13, 14))) {
      newES("abrada brada ba").minUniques(2, { x =>
        if (x < 6) { 6 }
        else if (x == 6) { 7 }
        else if (x < 12) { 12 }
        else if (x == 12) { 13 }
        else { 15 }
      })
    }
  }

}
