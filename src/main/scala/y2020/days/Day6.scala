package y2020.days

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

object Day6 {

  def run(): Unit = {
    val f = Source.fromFile("input/2020/day6.txt")
    println(process(f))
    f.close()
  }

  def process(f: BufferedSource): Int = {
    f.mkString
      .split("\n\n")
      .map(s => checkSum(s.split('\n')))
      .sum
  }

  def checkSum(answers: Array[String]): Int = {

    @tailrec
    def checkChar(acc: Int, char: Char): Int = char match {
      case 'z' => acc + testChar(char, answers)
      case _ => checkChar(testChar(char, answers) + acc, (char + 1).toChar)
    }

    checkChar(0, 'a')
  }

  def testChar(char: Char, answers: Array[String]): Int = {
    if (answers.exists(_.contains(char))) 1 else 0 //forall for part 1
  }

}
