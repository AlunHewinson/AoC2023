package aoc2015

import utils.utils.readDay

object day05 extends App {
  // https://adventofcode.com/2015/day/5

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2015)
    val inpSeparator: String = "\r*\n"
    val inputs: Seq[String] = inpSeparator.r.split(inp)

    def isNice1(x: String): Boolean = {
      """.*[aeiou].*[aeiou].*[aeiou].*""".r.matches(x) &
        """.*(.)\1.*""".r.matches(x) &
        !(""".*(ab|cd|pq|xy).*""".r.matches(x))
    }

    def isNice2(x: String): Boolean = {
      """.*(..).*\1.*""".r.matches(x) & """.*(.).\1.*""".r.matches(x)
    }

    val answer1 = inputs.map(isNice1).count(_==true)
    val answer2 = inputs.map(isNice2).count(_==true)

    (answer1, answer2)

  }

  println(solveDay(day = 5, test = "test"))
  println(solveDay(day = 5))

}
