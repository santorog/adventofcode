package main.scala.y2020.day1

import scala.io.{BufferedSource, Source}

object Day1 {

  def runDay1() : Unit = {
    val f = Source.fromFile("input/2020/day1.txt")
    println(Day1.process(f, 2))
   // println(Day1.process(f, 3))
    f.close()
  }

  def process(f: BufferedSource, i: Int): Int = {
    f.mkString.split('\n')
      .map(_.toInt)
      .combinations(i)
      .find(x => x.sum == 2020)
      .head.product
  }

}
