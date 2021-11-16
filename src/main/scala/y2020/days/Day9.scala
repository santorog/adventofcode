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
  def findFirstNotASum(numberSeq: Seq[Long], current: Int): Long = {
    if (!numberSeq.slice(current - 25, current).combinations(2).exists(x => x.sum == numberSeq(current)))
      numberSeq(current)
    else
      findFirstNotASum(numberSeq, current + 1)
  }

  def star2(numbers: Seq[Long]): Unit = {
    println(findContiguousSequence(numbers))
  }

  def findContiguousSequence(numberSeq: Seq[Long]): Long = {

    val numberToHack = findFirstNotASum(numberSeq, 25)

    @tailrec
    def boundsChange(startIndex: Int, endIndex: Int): Long = {
      val slice = numberSeq.slice(startIndex, endIndex)
      val sliceSum = slice.sum
      if (sliceSum < numberToHack)
        boundsChange(startIndex, endIndex + 1)
      else if (sliceSum > numberToHack)
        boundsChange(startIndex + 1, startIndex + 2)
      else
        slice.min + slice.max
    }

    boundsChange(0, 1)
  }

}