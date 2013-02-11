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

  import org.whym.growthring.{ExtremalSubstrings => ES}

  test("round min") {
    expectResult((3,3)) {
      ES.roundMin((5,7))
    }
    expectResult((5,5)) {
      ES.roundMin((10,12))
    }
  }

  test("string to unsigned: abc") {
    expectResult(List(0x61,0,0x62,0,0x63,0)) {
      ES.stringToUnsigneds("abc").toList
    }
  }

  test("uniques: abaababa") {
    expectResult(NES.minUniques("abaababa")){
      new ES("abaababa").minUniques
    }
  }

  test("repeats(2): abaababa") {
    expectResult(Seq((0,2), (3,5), (5,7))){
      new ES("abaababa").maxRepeats
    }
    expectResult(Seq((0,2), (3,5), (5,7))){
      new ES("abaababa").maxRepeats2
    }
  }

  test("uniques: banana") {
    expectResult(Seq((0,0), (2,4))){
      new ES("banana").minUniques
    }
  }

  test("repeats(2): banana") {
    expectResult(NES.maxRepeats("banana")) {
      new ES("banana").maxRepeats
    }
  }

  test("repeats(2): アブラカダブラ") {
    expectResult(NES.maxRepeats("アブラカダブラ")) {
      new ES("アブラカダブラ").maxRepeats
    }
  }

  // test("repeats(2): abradadabrara") {
  //   expectResult(Seq((1,2), (5,6))){
  //     new ES("abracadabrara").maxRepeats
  //   }
  // }

  test("repeats(3): aaa") {
    expectResult(Seq((0,0), (1,1), (2,2))){
      new ES("aaa").maxRepeats(3)
    }
  }

  test("repeats(3): banana") {
    expectResult(Seq((1,1), (3,3), (5,5))){
      new ES("banana").maxRepeats(3)
    }
  }

  test("repeats(3): bananan") {
    expectResult(Seq((1,2), (3,4), (5,6))){
      new ES("bananan").maxRepeats(3)
    }
  }

  test("repeats(4): bananan") {
    expectResult(Seq()){
      new ES("bananan").maxRepeats(4)
    }
  }

  test("repeats(4): bananan ban") {
    expectResult(NES.maxRepeats("bananan ban", 4)){
      new ES("bananan ban").maxRepeats(4)
    }
  }

  test("repeats(4): ACTATGAAGACAGGATCGATGCTA...") {
    expectResult(NES.maxRepeats("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA", 4)){
      new ES("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA").maxRepeats(4)
    }
  }

  test("repeats(4): 日本国民は...") {
    expectResult(NES.maxRepeats("日本国民は、正当に選挙された国会における代表者を通じて行動し、われらとわれらの子孫のために、諸国民との協和による成果と、わが国全土にわたって自由のもたらす恵沢を確保し、政府の行為によって再び戦争の惨禍が起ることのないやうにすることを決意し、ここに主権が国民に存することを宣言し、この憲法を確定する。そもそも国政は、国民の厳粛な信託によるものであって、その権威は国民に由来し、その権力は国民の代表者がこれを行使し、その福利は国民がこれを享受する。これは人類普遍の原理であり、この憲法は、かかる原理に基くものである。われらは、これに反する一切の憲法、法令及び詔勅を排除する。", 4)){
      new ES("日本国民は、正当に選挙された国会における代表者を通じて行動し、われらとわれらの子孫のために、諸国民との協和による成果と、わが国全土にわたって自由のもたらす恵沢を確保し、政府の行為によって再び戦争の惨禍が起ることのないやうにすることを決意し、ここに主権が国民に存することを宣言し、この憲法を確定する。そもそも国政は、国民の厳粛な信託によるものであって、その権威は国民に由来し、その権力は国民の代表者がこれを行使し、その福利は国民がこれを享受する。これは人類普遍の原理であり、この憲法は、かかる原理に基くものである。われらは、これに反する一切の憲法、法令及び詔勅を排除する。").maxRepeats(4)
    }
  }

  test("uniques(2): banana") {
    expectResult(NES.minUniques("banana", 2)){
      new ES("banana").minUniques2
    }
  }

  test("uniques(2): bananana") {
    expectResult(NES.minUniques("bananana", 2)){
      new ES("bananana").minUniques2
    }
  }

  test("uniques(2): ACTATGAAGACAGGATCGATGCTA...") {
    expectResult(NES.minUniques("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA", 2)){
      new ES("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA").minUniques2
    }
  }

  test("de Bruijn repeats(2): aaaabaabbababbbbaaa") {
    expectResult((0 to 16).toList.map(x => (x, x+2))) {
      new ES("aaaabaabbababbbbaaa").maxRepeats(2)
    }
  }

  test("de Bruijn uniques(2): aaaabaabbababbbbaaa") {
    expectResult((0 to 15).toList.map(x => (x, x+3))) {
      new ES("aaaabaabbababbbbaaa").minUniques
    }
  }

}
