package aoc2022

import utils.utils.readDay

import scala.util.matching.Regex

object day01 extends App {
  // https://adventofcode.com/2022/day/1

  def solveDay(day: Int, test: Boolean = false): Int = {

    val inp: String = readDay(day, test, year = 2022)
    val elfSeparator: Regex = "(\r*\n){2,}".r
    val snackSeparator: Regex = "\r*\n".r

    val elves: Array[String] = elfSeparator.split(inp)
    val elfCalories: Array[Int] = elves.
      map(elf => snackSeparator.split(elf).map(_.toInt).sum)

    val n = 3
    elfCalories.sorted(Ordering[Int].reverse).take(n).sum

  }

  println(solveDay(1, true))
  println(solveDay(1))

}
