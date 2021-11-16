package y2020.days

import scala.annotation.tailrec
import scala.io.Source

object Day9 {

  def run(): Unit = {
    val f = Source.fromFile("input/2020/day9.txt")
    val numbers = f.mkString.split("\n").map(_.toLong)
    star1(numbers)
    star2(numbers)
    f.close()
  }

  def star1(numbers: Seq[Long]): Unit = {
    println(findFirstNotASum(numbers, 25))
  }

  @tailrec
  def findFirstNotASum(numbers: Seq[Long], current: Int): Long = {
    if (!numbers.slice(current - 25, current).combinations(2).exists(x => x.sum == numbers(current)))
      numbers(current)
    else
      findFirstNotASum(numbers, current + 1)
  }

  def star2(numbers: Seq[Long]): Unit = {
    println(findContiguousSequence(numbers))
  }

  def findContiguousSequence(numbers: Seq[Long]): Long = {

    val numberToHack = findFirstNotASum(numbers, 25)

    @tailrec
    def boundsChangeSolver(startIndex: Int, endIndex: Int): Long = {
      val slice = numbers.slice(startIndex, endIndex)
      val sliceSum = slice.sum
      if (sliceSum < numberToHack)
        boundsChangeSolver(startIndex, endIndex + 1)
      else if (sliceSum > numberToHack)
        boundsChangeSolver(startIndex + 1, startIndex + 2)
      else
        slice.min + slice.max
    }

    boundsChangeSolver(0, 1)
  }

}