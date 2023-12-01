package aoc2023

import utils.utils.readDay

import scala.collection.immutable.Seq
import scala.util.matching.Regex

object day01 extends App {
  // https://adventofcode.com/2023/day/1

  def solveDay(day: Int, test: Boolean = false): Int = {

    val inp: String = readDay(day, test, year = 2023)
    val instructionSeparator: String = "\r*\n"
    val instructions: Array[String] = instructionSeparator.r.split(inp)

    instructions.map(extractFirstAndLastDigits).foreach(println)
    instructions.map(extractFirstAndLastDigits).sum

  }

  private def extractFirstAndLastDigits(inputString: String): Int = {
    val elfNumbers: List[String] = List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "zero", "one", "two", "three",
      "four", "five", "six", "seven", "eight", "nine")

    //val elfMatches = elfNumbers.map(elfNumber => elfNumber.r.findAllMatchIn(inputString))
    val elfMatchesNumbers1: Seq[Iterator[Regex.Match]] = elfNumbers.map(elfNumber => elfNumber.r.findAllMatchIn(inputString))
    val elfMatchesNumbers2: Seq[Iterator[Regex.Match]] = elfNumbers.map(elfNumber => elfNumber.r.findAllMatchIn(inputString))
    val elfMatchesStrings: Seq[Regex.MatchIterator] = elfNumbers.map(elfNumber => elfNumber.r.findAllIn(inputString))
    val minStarts: Seq[Int] = elfMatchesNumbers1.map(matches => {
      if (matches.isEmpty) {
        1000000
      } else {
        matches.map(lign => {
          lign.start
        }).min
      }
    })
//    println(minStarts)
    val maxStarts: Seq[Int] = elfMatchesNumbers2.map(matches => {
      if (matches.isEmpty) {
        -1
      } else {
        matches.map(yo => {
          yo.start
        }).max
      }
    })
//    println(maxStarts)
    val matches = elfMatchesStrings.map(matches => {
      if (matches.isEmpty) {
        ""
      } else {
        matches.map(yo => {
          yo
        }).min
      }
    })

    val firstNumberIndex = minStarts.zipWithIndex.minBy(_._1)._2
    val lastNumberIndex = maxStarts.zipWithIndex.maxBy(_._1)._2

    val elfDigitsMap: Map[String, Int] = Map(
      "0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9, "zero" -> 0,
      "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9
    )
    97
    elfDigitsMap.getOrElse(matches(firstNumberIndex), 0) * 10 + elfDigitsMap.getOrElse(matches(lastNumberIndex), 0)
  }

  println(solveDay(1, true))
  println(solveDay(1))

}
