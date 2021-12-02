package y2021

import scala.annotation.tailrec
import scala.io.Source

object Day2 {

  def run(): Unit = {
    val f = Source.fromFile("input/2021/day2.txt")
    val depths = f.mkString.split('\n').map(parseCommand).toList
    println(star1(depths))
    println(star2(depths))
    f.close()
  }

  val F = "forward"
  val U = "up"
  val D = "down"

  def parseCommand(instruction: String): (String, Int) = {
    val pattern = ("(" + F + "|" + U + "|" + D + ") ([1-9])").r
    val pattern(op, shift) = instruction
    (op, shift.toInt)
  }

  def star1(depths: Seq[(String, Int)]): Int = {
    val sums = depths.groupBy(_._1).map(seq => seq._2.reduce((x, y) => (x._1, x._2 + y._2)))
    (sums(D) - sums(U)) * sums(F)
  }

  def star2(depths: List[(String, Int)]): Int = {

    @tailrec
    def process(aim: Int, x: Int, y: Int, instructions: List[(String, Int)]): Int =
      instructions match {
        case Nil => x * y
        case t :: q =>
          t match {
            case (F, i) => process(aim, x + i, y + aim * i, q)
            case (U, i) => process(aim - i, x, y, q)
            case (D, i) => process(aim + i, x, y, q)
          }
      }

    process(0, 0, 0, depths)
  }

}
