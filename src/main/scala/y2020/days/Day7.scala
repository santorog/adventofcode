package y2020.days

import scala.io.{BufferedSource, Source}

object Day7 {

  def run(): Unit = {
    val f = Source.fromFile("input/2020/day7.txt")
    process(f)
    f.close()
  }

  def process(f: BufferedSource): Unit = {
    val rules = parseRulesFromFile(f)
    question1(rules)
    question2(rules)
  }

  def parseRulesFromFile(f: BufferedSource): Map[String, Set[(String, Int)]] = {
    f.mkString
      .split("\n")
      .map(rule => rule.split(" bags contain "))
      .map(stringArray => stringArray(0) -> parseBagList(stringArray(1)))
      .toMap
  }

  def parseBagList(bags: String): Set[(String, Int)] = {
    " bag[s]?[|,.][ ]?".r.split(bags)
      .filter(s => s.nonEmpty && !s.equals("no other"))
      .map(s => parseBagAndOccurences(s))
      .toSet
  }

  def parseBagAndOccurences(bags: String) : (String, Int) = {
    val pattern = "([0-9]+) (.*)".r
    val pattern(count, bagType) = bags
    (bagType, count.toInt)
  }

  def question1(rules: Map[String, Set[(String, Int)]]): Unit = {
    println(findOuterBags(rules, "shiny gold").size)
  }

  def findOuterBags(rules: Map[String, Set[(String, Int)]], myBag: String): Set[String] = {

    def analyzeRecursively(rules: Map[String, Set[(String, Int)]], myBag: String, acc: Set[String]): Set[String] = {

      val outerBagCandidates = rules.filter(rule => rule._2.exists(innerBag => innerBag._1.equals(myBag)))
      if (outerBagCandidates.isEmpty) acc
      else
        outerBagCandidates
          .flatMap(s => analyzeRecursively(rules, s._1, acc + s._1))
          .toSet
    }
   analyzeRecursively(rules, myBag, Set.empty)
  }

  def question2(rules: Map[String, Set[(String, Int)]]): Unit = {
    println()

  }

}