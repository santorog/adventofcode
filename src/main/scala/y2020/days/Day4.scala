package y2020.days

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex

object Day4 {

  def run(): Unit = {
    val f = Source.fromFile("input/2020/day4.txt")
    println(process(f))
    f.close()
  }

  def process(f: BufferedSource): Int = {
    f.mkString
      .split("\n\n")
      .map(s => checkSum(s))
      .count(_ >= 7)
  }

  def checkSum(passport: String): Int = {
    passport.split("[\n ]")
      .map {
        case s"${k}:${v}" => validateField(patternFor(k), v)
        case _ => 0
      }.sum
  }

  def patternFor(key: String): Regex =
    key match {
      case "byr" => "19[2-9][0-9]|200[0-2]".r
      case "iyr" => "20(1[0-9]|20)".r
      case "eyr" => "20(2[0-9]|30)".r
      case "hgt" => "(1([5-8][0-9]|9[0-3]))cm|(59|6[0-9]|7[0-6])in".r
      case "hcl" => "#[0-9a-f]{6}".r
      case "ecl" => "amb|blu|brn|gry|grn|hzl|oth".r
      case "pid" => "[0-9]{9}".r
      case _ => "".r
    }

  def validateField(r: Regex, v: String): Int = if (r.pattern.matcher(v).matches()) 1 else 0
}
