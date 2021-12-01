package y2021

import scala.io.Source

object Day1 {

  def run(): Unit = {
    val f = Source.fromFile("input/2021/day1.txt")
    val depths = f.mkString.split('\n').map(_.toInt)
    println(star1(depths))
    println(star2(depths))
    f.close()
  }

  def star1(depths: Seq[Int]): Int = {
    depths.sliding(2).map(a => a(1) - a(0)).count(v => v > 0)
  }

  def star2(depths: Seq[Int]): Int = {
    depths.sliding(4).map(a => a(3) - a(0)).count(v => v > 0)
  }

}
