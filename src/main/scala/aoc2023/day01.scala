package aoc2023

import utils.utils.readDay

import scala.collection.immutable.Seq
import scala.util.matching.Regex.{Match, MatchIterator}

object day01 extends App {
  // https://adventofcode.com/2023/day/1

  def solveDay(day: Int, test: Boolean = false): Int = {

    val inp: String = readDay(day, test, year = 2023)
    val instructionSeparator: String = "\r*\n"
    val instructions: Array[String] = instructionSeparator.r.split(inp)

    instructions.map(extractFirstAndLastDigits).sum

  }

  private def extractFirstAndLastDigits(inputString: String): Int = {
    val elfNumbers: List[String] = List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "zero", "one", "two", "three",
      "four", "five", "six", "seven", "eight", "nine")

    val elfMatchNumbers: Seq[Iterator[Match]] = elfNumbers.map(elfNumber => elfNumber.r.findAllMatchIn(inputString))
    val elfMatchStrings: Seq[MatchIterator] = elfNumbers.map(elfNumber => elfNumber.r.findAllIn(inputString))
    val minMaxStarts: Seq[(Int, Int)] = elfMatchNumbers.map(matches => {
      if (matches.isEmpty) {
        (1000, -1)
      } else {
        val matchStarts: List[Int] = matches.map(x => {x.start}).toList
        (matchStarts.min, matchStarts.max)
      }
    })
    val matches = elfMatchStrings.map(matches => {
      if (matches.isEmpty) {
        ""
      } else {
        matches.toList.head
      }
    })

    val firstNumberIndex = minMaxStarts.zipWithIndex.minBy(_._1._1)._2
    val lastNumberIndex = minMaxStarts.zipWithIndex.maxBy(_._1._2)._2

    val elfDigitsMap: Map[String, Int] = Map(
      "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9,  "one" -> 1,
      "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9
    )

    elfDigitsMap.getOrElse(matches(firstNumberIndex), 0) * 10 + elfDigitsMap.getOrElse(matches(lastNumberIndex), 0)
  }

  println(solveDay(1, test = true))
  println(solveDay(1))

}
