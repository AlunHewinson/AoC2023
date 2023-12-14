package aoc2015

import utils.utils.readDay

object day01 extends App {
  // https://adventofcode.com/2015/day/1

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2015)

    val upsAndDowns: Seq[Int] = inp.map {
      case x if x == "(".head => 1
      case x if x == ")".head => -1
      case x                  => {
        println(x)
        0
      }
    }

    val answer1 = upsAndDowns.sum
    val answer2 = upsAndDowns.foldLeft(Seq(0))((acc, curr) => acc :+ (acc.last + curr)).tail.indexOf(-1) + 1

    (answer1, answer2)

  }

  println(solveDay(day = 1, test = "test"))
  println(solveDay(day = 1))

}
