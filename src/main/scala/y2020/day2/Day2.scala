package y2020.day2

import scala.io.{BufferedSource, Source}

object Day2 {

  def runDay2(): Unit = {
    val f = Source.fromFile("input/2020/day2.txt")
    println(Day2.process(f))
    f.close()
  }

  def process(f: BufferedSource): Int = {
    f.mkString.split('\n').
      map {
        case s"${min}-${max} ${l}: ${password}" => part2(min.toInt, max.toInt, l.charAt(0), password)
        case _ => 0
      }.sum
  }

  def part1(low: Int, high: Int, l: Char, pd: String): Int = {
    val a = pd.count(_ == l)
    if (a >= low && high >= a) 1
    else 0
  }

  def part2(low: Int, high: Int, l: Char, pd: String): Int = {
    if (test(low, l, pd) && test(high, l, pd) || !test(low, l, pd) && !test(high, l, pd)) 0 else 1
  }

  def test(pos: Int, l: Char, pd: String): Boolean = {
    if (pos > pd.length || pos < 1) false else pd.charAt(pos - 1) == l
  }

}
