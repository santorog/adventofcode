package y2020

import scala.io.{BufferedSource, Source}

object Day5 {

  def run(): Unit = {
    val f = Source.fromFile("input/2020/day5.txt")
    println(process(f))
    f.close()
  }

  def process(f: BufferedSource): Unit = {
    val a = f.mkString
      .split("\n")
      .map(s => checkSum(s))
      .sorted
    for (i <- a.indices) {
      if ((a(i) + 1) != a(i + 1)) println(a(i) + " " + a(i + 1))

    }
  }

  def checkSum(seat: String): Int = {

    val a = seat.toCharArray
    var rowIncr = 64
    var colIncr = 4
    var rowRes = 0
    var colRes = 0
    for (i <- 0 to 6) {
      if (a(i) == 'B') rowRes += rowIncr
      rowIncr /= 2
    }
    for (i <- 7 to 9) {
      if (a(i) == 'R') colRes += colIncr
      colIncr /= 2
    }
    id(rowRes, colRes)
  }

  def id(row: Int, col: Int): Int = row * 8 + col

}
