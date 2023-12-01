package aoc2023

import utils.utils.readDay

object day02 extends App {
  // https://adventofcode.com/2023/day/2

  def solveDay(day: Int, test: Boolean = false): Int = {

    val inp: String = readDay(day, test, year = 2023)
    val instructionSeparator: String = "\r*\n"
    val instructions: Array[String] = instructionSeparator.r.split(inp)
    instructions.map(extractSomething).sum

  }

  private def extractSomething(inputString: String): Int = {
    98
  }

  println(solveDay(1, true))
  //println(solveDay(1))

}
