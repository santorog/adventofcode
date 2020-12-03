package y2020.day3

import scala.annotation.tailrec
import scala.io.Source


object Day3 {

  def runDay3(): Unit = {
    val f = Source.fromFile("input/2020/day3.txt")
    val map: Array[Array[Char]] = f.mkString.split('\n').map(_.toCharArray)

    val a = calculate(map, 1, 1)
    val b = calculate(map, 3, 1)
    val c = calculate(map, 5, 1)
    val d = calculate(map, 7, 1)
    val e = calculate(map, 1, 2)

    println(a * b * c * d * e)

    f.close()
  }


  def calculate(map: Array[Array[Char]], r: Int, d: Int): Long = {
    @tailrec
    def acc(x: Int, y: Int, h: Int, w: Int, res: Long): Long = {
      y match {
        case v if v > h - 1 => res
        case _ => acc((x + r) % w, y + d, h, w,
          res + (if (map(y)(x) == '#') 1 else 0))
      }
    }

    acc(r, d, map.length, map(0).length, 0)
  }
}


