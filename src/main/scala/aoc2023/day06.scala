package aoc2023

import utils.utils.readDay

import scala.annotation.tailrec

object day06 extends App {
  // https://adventofcode.com/2023/day/6

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val lineSeparator: String = "\r*\n"
    val lines: Array[String] = lineSeparator.r.split(inp)
    val duration = " +".r.split(lines.head).tail.map(_.toInt)
    val record = " +".r.split(lines.tail.head).tail.map(_.toInt)
    val durationRecord = duration.zip(record)

    val winningPossibilities = durationRecord.map(dr => {
      val r = Range(0, dr._1)
      val distances = r.map(t => t * (dr._1 - t))
      distances.count(_ > dr._2)
    })

    val answer1 = winningPossibilities.product

    val duration2 = BigInt(duration.mkString(""))
    val record2 = BigInt(record.mkString(""))

    def testWin(x: BigInt, duration: BigInt, record: BigInt): Boolean = {
      x * (duration - x) > record
    }

    def halfStep(from: BigInt, to: BigInt): BigInt = {
      val step = (from - to)/2
      if (step==0) from - (from - to).sign else from - step
    }

    @tailrec
    def binarySearchLow(lo: BigInt, hi: BigInt, accum: (BigInt, BigInt)): (Boolean, BigInt, BigInt) = {
      //println(s"lo: $lo   hi: $hi")
      val testLo: Boolean = testWin(lo, duration2, record2)
      val testHi: Boolean = testWin(hi, duration2, record2)
      val testDf: BigInt = hi - lo

      (testLo, testHi, testDf) match {
        case (false, true, n) if n == 1 => (true, lo, hi)
        case (false, true, n) => binarySearchLow(halfStep(lo, lo + (hi-lo)/2), halfStep(lo + (hi-lo)/2, hi), (lo, hi))
        case (false, false, n) => binarySearchLow(hi, accum._2, (lo, hi))
        case (true, true, n) => binarySearchLow(accum._1, lo, (lo, hi))
        case (true, false, n) => (false, -1, -1)
      }
    }

    @tailrec
    def binarySearchHigh(lo: BigInt, hi: BigInt, accum: (BigInt, BigInt)): (Boolean, BigInt, BigInt) = {
      //println(s"lo: $lo   hi: $hi")
      val testLo: Boolean = testWin(lo, duration2, record2)
      val testHi: Boolean = testWin(hi, duration2, record2)
      val testDf: BigInt = hi - lo

      (testLo, testHi, testDf) match {
        case (true, false, n) if n == 1 => (true, lo, hi)
        case (true, false, n) => binarySearchHigh(halfStep(lo, lo + (hi-lo)/2), halfStep(lo + (hi-lo)/2, hi), (lo, hi))
        case (true, true, n) => binarySearchHigh(hi, accum._2, (lo, hi))
        case (false, false, n) => binarySearchHigh(accum._1, lo, (lo, hi))
        case (false, true, n) => (false, -1, -1)
      }
    }

    val lowWin = binarySearchLow(0, duration2/2, (0, duration2/2))._3
    val highWin = binarySearchHigh(duration2/2, duration2, (duration2/2, duration2))._2

    val answer2 = 1 + highWin - lowWin

    (answer1, answer2)

  }

  println(solveDay(6, test = true))
  println(solveDay(6))

}
