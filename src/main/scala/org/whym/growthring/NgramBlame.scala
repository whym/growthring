/**
 *
 * @author Yusuke Matsubara <whym@whym.org>
 *
 */

package org.whym.growthring

import scala.collection.JavaConverters._
import scala.collection.mutable

object NgramBlame {

  def concat(spans: Seq[(Int,Int,Int)]): Seq[(Int,Int,Int)] = {
    val ret = new mutable.ArrayBuffer[(Int,Int,Int)]
    for ( s <- spans ) {
      if ( ret.size > 0 &&  ret.last._3 == s._3  &&  ret.last._2 == s._1 ) {
        ret(ret.size-1) = (ret.last._1, s._2, s._3)
      } else {
        ret.append(s)
      }
    }
    ret
  }

  def blameGreedy(body: String, revs: IndexedSeq[String], n: Int): Set[(Int, Int, Int)] = {
    val ng = new NgramQueue[Char](n, n)
    val pos = new mutable.HashMap[Seq[Char], Int]
    for ( (str,i) <- revs.zipWithIndex;
          s <- str ) yield {
      ng enqueue s
      for ( x <- ng.getNgrams.map(_.toSeq) ) {
        pos(x) = pos.getOrElse(x, -1) max i
      }
    }
    var m = -1
    concat((for ( i <- 0 to (body.size - n) ) yield {
      val r = pos.getOrElse(body.slice(i, i+n), -1)
      (i, i+n, r)
    }).filter(_ match {case (_,_,r) => r >= 0}).map{
      r => {
        val s = m max r._1
        val rr = (s, r._2, r._3)
        m = m max r._2
        rr
      }
    }.filter(x => x._1 < x._2)).toSet
  }

}
