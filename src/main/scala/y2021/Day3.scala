package y2021

import scala.annotation.tailrec
import scala.io.Source

object Day3 {

  def run(): Unit = {
    val f = Source.fromFile("input/2021/day3.txt")
    val input = f.mkString.split('\n')
    println(star1(input))
    println(star2(input))
    f.close()
  }

  def star1(input: Seq[String]): Int = {

    val halfSize = input.size / 2

    var gammaRateStr = ""
    for (i <- input.head.indices) {
      val nextBit = if (input.map(s => s(i)).count(_.equals('1')) > halfSize) '1' else '0'
      gammaRateStr = gammaRateStr.appended(nextBit)
    }

    val gammaRate = Integer.parseInt(gammaRateStr, 2)
    val epsilonRate = Math.pow(2, gammaRateStr.length).toInt - 1 - gammaRate
    gammaRate * epsilonRate
  }

  def star2(input: Seq[String]): Int = {

    @tailrec
    def aux(input: Seq[String], index: Int, isOxygenRating: Boolean): Int = {
      if (input.size == 1) return Integer.parseInt(input.head, 2)

      val halfSize = input.size / 2.0
      val test = (x: Int) => if (x == halfSize) isOxygenRating else if (isOxygenRating) x > halfSize else x < halfSize

      val nextBit = if (test(input.map(s => s(index)).count(_.equals('1')))) '1' else '0'
      val nextInput = input.filter(s => s(index) == nextBit)
      aux(if (nextInput.isEmpty) input else nextInput, index + 1, isOxygenRating)
    }

    aux(input, 0, isOxygenRating = true) * aux(input, 0, isOxygenRating = false)
  }
}
