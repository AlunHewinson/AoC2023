package aoc2023

import utils.utils.readDay

import scala.math.max

object day02 extends App {
  // https://adventofcode.com/2023/day/2

  def solveDay(day: Int, test: Boolean = false): Int = {

    val inp: String = readDay(day, test, year = 2023)
    val gameSeparator: String = "\r*\n"
    val games: Array[String] = gameSeparator.r.split(inp)
    val roundSeparator: String = "[:;]"
    val gameRounds = games.map(game => roundSeparator.r.split(game))
    val pickSeparator: String = ","
    val gameRoundPicks = gameRounds.map(game => game.map(round => pickSeparator.r.split(round)))

    def rgbify(colourNumber: String): (Int, Int, Int) = {
      colourNumber match {
        case s" $number red"   => (number.toInt, 0, 0)
        case s" $number green" => (0, number.toInt, 0)
        case s" $number blue"  => (0, 0, number.toInt)
      }
    }

    def maximiser(accumulator: (Int, Int, Int), toTest: (Int, Int, Int)): (Int, Int, Int) = {
      (max(accumulator._1, toTest._1), max(accumulator._2, toTest._2), max(accumulator._3, toTest._3))
    }

    val gameRoundMaximums: Array[Array[(Int, Int, Int)]] = gameRoundPicks.map(game => {
      val rounds = game.tail
      rounds.map(round => {
        round.foldLeft((0, 0, 0))((acc, nums) => maximiser(acc, rgbify(nums)))
      })
    })

    val maxAllowds = (12, 13, 14)
    val gameScores: Array[(Int, Int, Int)] = gameRoundMaximums.map(game => {
      game.foldLeft(maxAllowds)((acc, nums) => maximiser(acc, nums))
    })

    val validGamesScore: Int = gameScores.zipWithIndex.map(indexedGame => {
      if (indexedGame._1 == maxAllowds) indexedGame._2 + 1 else 0
    }).sum

    val gamePowerSum: Int = gameRoundMaximums.map(game => {
      game.foldLeft((0, 0, 0))((acc, nums) => maximiser(acc, nums))
    }).map(game => {
      game._1 * game._2 * game._3
    }).sum

    //validGamesScore // part 1
    gamePowerSum // part 2
  }


  println(solveDay(2, test = true))
  println(solveDay(2))

}
