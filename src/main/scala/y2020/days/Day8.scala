package y2020.days

import y2020.days.Day8.Command.{ACC, JMP, NOP}

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}
import scala.util.control.Breaks.{break, breakable}

object Day8 {

  class Command(val op: String, val value: Int) {

    def execute(line: Int, acc: Int): (Int, Int) = {
      op match {
        case ACC => (line + 1, acc + value)
        case JMP => (line + value, acc)
        case NOP => (line + 1, acc)
      }
    }

    def switch(): Command = {
      op match {
        case NOP => new Command(JMP, value)
        case JMP => new Command(NOP, value)
        case _ => this
      }
    }

  }

  object Command {
    val ACC = "acc"
    val JMP = "jmp"
    val NOP = "nop"
  }

  def run(): Unit = {
    val f = Source.fromFile("input/2020/day8.txt")
    val script = parseScript(f)
    star1(script)
    star2(script)
    f.close()
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

  @tailrec
  def runScriptSafely(script: Seq[Command], currentLine: Int, acc: Int, visitedLines: Set[Int]): (Int, Int) = {
    if (visitedLines.contains(currentLine) || script.length <= currentLine) return (currentLine, acc)

    val params = script(currentLine).execute(currentLine, acc)
    runScriptSafely(script, params._1, params._2, visitedLines + currentLine)
  }

  def star1(script: Seq[Command]): Unit = {
    println(runScriptSafely(script, 0, 0, Set.empty)._2)
  }

  def star2(script: Seq[Command]): Unit = {

    for (index <- script.indices) {
      val command = script(index)
      breakable {
        if (command.op.equals(ACC)) break
      }
      val result = runScriptSafely(script.updated(index, command.switch()), 0, 0, Set.empty)
      if (result._1 >= script.length) println(result._2)
    }
  }

}
