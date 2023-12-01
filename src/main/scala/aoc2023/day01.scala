package aoc2023

import utils.utils.readDay

object day01 extends App {
  // https://adventofcode.com/2023/day/1

  def solveDay(day: Int, test: Boolean = false): Int = {

    val inp: String = readDay(day, test, year = 2023)
    val instructionSeparator: String = "\r*\n"
    val instructions: Array[String] = instructionSeparator.r.split(inp)
    instructions.map(extractFirstAndLastDigits).sum

  }

  private def extractFirstAndLastDigits(inputString: String): Int = {
    val elfDigits: String = "0|1|2|3|4|5|6|7|8|9|zero|one|two|three|four|five|six|seven|eight|nine"
    val foundElfMatches: Seq[String] = elfDigits.r.findAllIn(inputString).toList //findAllMatchIn(inputString).toList
    val elfDigitsMap: Map[String, Int] = Map(
      "0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9, "zero" -> 0,
      "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9
    )
    elfDigitsMap.getOrElse(foundElfMatches.head, 0) * 10 + elfDigitsMap.getOrElse(foundElfMatches.last, 0)
  }

  println(solveDay(1, true))
  println(solveDay(1))

}
