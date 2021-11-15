package y2020.days

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

object Day8 {

  class Command(val op: String, val value: Int) {

    def execute(line: Int, acc: Int): (Int, Int) = {
      op match {
        case "acc" => (line + 1, acc + value)
        case "jmp" => (line + value, acc)
        case "nop" => (line + 1, acc)
      }
    }
  }

  def run(): Unit = {
    val f = Source.fromFile("input/2020/day8.txt")
    process(f)
    f.close()
  }

  def process(f: BufferedSource): Unit = {
    val script = parseScript(f)
    star1(script)
  }

  def parseScript(bufferedSource: BufferedSource): Seq[Command] = {
    bufferedSource.mkString
      .split("\n")
      .map(line => parseCommand(line))
  }

  def parseCommand(scriptLine: String): Command = {
    val pattern = "(acc|jmp|nop) ([+\\-][0-9]+)".r
    val pattern(op, shift) = scriptLine
    new Command(op, shift.toInt)
  }

  def star1(script: Seq[Command]): Unit = {

    @tailrec
    def aux(script: Seq[Command], commandLine: Int, acc: Int, visitedLines: Set[Int]) : Int = {
      if (visitedLines.contains(commandLine)) return acc

      val params = script(commandLine).execute(commandLine, acc)
      aux(script, params._1, params._2, visitedLines + commandLine)
    }

    println(aux(script, 0, 0, Set.empty))
  }

}
