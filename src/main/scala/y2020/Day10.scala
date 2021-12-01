package y2020

import scala.annotation.tailrec
import scala.io.Source

object Day10 {

    def run(): Unit = {
      val f = Source.fromFile("input/2020/day10.txt")
      val numbers = f.mkString.split("\n").map(_.toLong).sorted
      star1(numbers)
      star2(numbers)
      f.close()
    }

  def star1(numbers: Seq[Long]): Unit = {

    @tailrec
    def aux(oneJolt: Int, threeJolt: Int, index: Int): Int = {

      if (index == numbers.length) return oneJolt * (threeJolt + 1) // + 1 as we need to add the built-in 3J adapter.

      val diff = if (index == 0) numbers(index) else numbers(index) - numbers(index - 1)
      diff match {
        case 1 => aux(oneJolt + 1, threeJolt, index + 1)
        case 3 => aux(oneJolt, threeJolt + 1, index + 1)
        case _ => aux(oneJolt, threeJolt, index + 1)
      }
    }

    println(aux(0, 0, 0))
  }

    def star2(numbers: Seq[Long]): Unit = {

      val maxIndex = numbers.length - 1
      def aux(index: Int): Int = {

        if (index == maxIndex) return 1

        val c2 = if ( index + 2 <= maxIndex && numbers(index + 2) - numbers(index) <= 3 ) aux(index + 2) else 0
        val c3 = if ( index + 3 <= maxIndex && numbers(index + 3) - numbers(index) <= 3 ) aux(index + 3) else 0

        aux(index + 1) + c2 + c3
      }

      println(aux(0) + aux(1))
    }

}
