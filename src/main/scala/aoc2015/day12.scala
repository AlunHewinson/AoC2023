package aoc2015

import utils.utils.readDay

object day12 extends App {
  // https://adventofcode.com/2015/day/12

  def solveDay(day: Int, test: String = "test"): (Int, Int) = {

    val inp: String = readDay(day, test, year = 2023)
    val gameSeparator: String = "\r*\n"
    val games: Array[String] = gameSeparator.r.split(inp)

    (98, 98)

  }

  println(solveDay(12, test = "test"))
  println(solveDay(12))

}
