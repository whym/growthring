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

  //! 1個だけ合わない
// List((0,3), (2,4), (4,6), (5,7), (7,9), (8,11), (10,13), (11,14), (12,15), (14,16), (15,18), (16,19), (17,20), (18,21), (19,22), (20,23), (22,24), (23,25), (24,27), (25,28), (26,29), (27,30), (28,31), (29,32), (31,33), (32,35), (33,37), (34,38), (36,39), (37,40), (39,41), (40,43), (41,44), (42,45), (43,46), (44,47), (45,48), (47,49), (48,50), (49,51), (50,53), (53,55), (54,56), (55,58), (56,59), (57,60), (60,63), (61,64), (62,65), (63,66), (65,68), (66,69), (67,70), (68,71), (69,72), (70,73), (72,74), (73,76), (76,78), (77,80), (79,81), (80,83), (81,84), (83,86), (84,87), (85,88), (87,89), (88,91), (90,92), (91,93), (92,94), (93,96), (95,97), (96,98), (98,101), (101,104), (103,105), (104,106), (105,108), (106,109), (108,111), (109,112), (110,113), (111,114), (113,116), (114,117), (115,118), (117,119), (118,120), (119,122), (120,123), (121,124), (122,125), (123,126), (126,129), (127,130), (128,131), (130,132), (132,134), (133,135), (134,136), (135,139), (138,140), (139,142), (140,143), (141,144), (142,145), (143,146), (144,147), (146,148), (147,149), (148,150), (149,151), (150,153), (153,156), (154,157), (156,158), (157,159), (159,161), (160,163), (163,165), (165,167), (166,169), (167,170), (169,171), (172,174), (174,176), (175,177), (176,178), (178,181), (179,182), (181,183), (182,185), (183,186), (184,187), (185,188), (186,189), (188,190), (190,192), (191,194), (192,195), (193,196), (194,197), (195,198)), 
// List((0,3), (2,4), (4,6), (5,7), (7,9), (8,11), (10,13), (11,14), (12,15), (14,16), (15,18), (16,19), (17,20), (18,21), (19,22), (20,23), (22,24), (23,25), (24,27), (25,28), (26,29), (27,30), (28,31), (29,32), (31,33), (32,35), (33,37), (34,38), (36,39), (37,40), (39,41), (40,43), (41,44), (42,45), (43,46), (44,47), (45,48), (47,49), (48,50), (49,51), (50,53), (53,55), (54,56), (55,58), (56,59), (57,60), (60,63), (61,64), (62,65), (63,66), (65,68), (66,69), (67,70), (68,71), (69,72), (70,73), (72,74), (73,76), (76,78), (77,80), (79,81), (80,83), (81,84), (83,86), (84,87), (85,88), (87,89), (88,91), (90,92), (91,93), (92,94), (93,96), (95,97), (96,98), (98,101), (101,104), (103,105), (104,106), (105,108), (106,109), (108,111), (109,112), (110,113), (111,114), (113,116), (114,117), (115,118), (117,119), (118,120), (119,122), (120,123), (121,124), (122,125), (123,126), (126,129), (127,130), (128,131), (130,132), (132,134), (133,135), (134,136), (135,139), (138,140), (139,142), (140,143), (141,144), (142,145), (143,146), (144,147), (146,148), (147,149), (148,150), (149,151), (150,153), (153,156), (154,157), (156,158), (157,159), (159,161), (160,163), (163,165), (165,167), (166,169), (167,170), (169,171), (172,174), (174,176), (175,177), (176,178), (178,181), (179,182), (181,183), (182,185), (183,186), (184,187), (185,188), (186,189), (188,190), (188,191), (190,192), (191,194), (192,195), (193,196), (194,197), (195,198)) 
  // test("uniques(2): ACTATGAAGACAGGATCGATGCTA...") {
  //   expect(myMinUniques("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA", 2)){
  //     new ES("ACTATGAAGACAGGATCGATGCTAATTGGCGGAGGGGGGCTTCCGCGCGTGACGAGTCCGGCCTCGGCGATGGTACAGACTGGGCCCTATTGTTTCGTACGGCCCATTCTCCTCTCGCTTTGGTCGGCCGACCCATACGAAGGCTACAAACCGGCCTAAAGTCTCAGCGCACAGCAATACGGTTGCCGCACTGCGGACGA").minUniques2
  //   }
  // }

}
