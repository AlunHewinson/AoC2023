package aoc2023

import utils.Address
import utils.utils.readDay

import scala.annotation.tailrec

object day16 extends App {
  // https://adventofcode.com/2023/day/16

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val lineSeparator: String = "\r*\n"
    val lines: Array[String] = lineSeparator.r.split(inp)

    @tailrec
    def propagate(acMap: Map[Address, Char], maxes: (Int, Int), acc: Seq[AddressDirection] = Seq()): Seq[AddressDirection] = {

      val newAcc: Seq[AddressDirection] = acc ++ acc.flatMap(ad => {
        val currentSymbol = acMap.getOrElse(ad.a, ".")
        val resultants: Seq[AddressDirection] = (ad.d, currentSymbol.toString) match {
          case (0, "-") => Seq(AddressDirection(ad.a.east(), 1), AddressDirection(ad.a.west(), 3))
          case (0, "|") => Seq(AddressDirection(ad.a.north(), 0))
          case (0, "/") => Seq(AddressDirection(ad.a.east(), 1))
          case (0, "\\") => Seq(AddressDirection(ad.a.west(), 3))
          case (0, ".") => Seq(AddressDirection(ad.a.north(), 0))

          case (1, "-") => Seq(AddressDirection(ad.a.east(), 1))
          case (1, "|") => Seq(AddressDirection(ad.a.north(), 0), AddressDirection(ad.a.south(), 2))
          case (1, "/") => Seq(AddressDirection(ad.a.north(), 0))
          case (1, "\\") => Seq(AddressDirection(ad.a.south(), 2))
          case (1, ".") => Seq(AddressDirection(ad.a.east(), 1))

          case (2, "-") => Seq(AddressDirection(ad.a.east(), 1), AddressDirection(ad.a.west(), 3))
          case (2, "|") => Seq(AddressDirection(ad.a.south(), 2))
          case (2, "/") => Seq(AddressDirection(ad.a.west(), 3))
          case (2, "\\") => Seq(AddressDirection(ad.a.east(), 1))
          case (2, ".") => Seq(AddressDirection(ad.a.south(), 2))

          case (3, "-") => Seq(AddressDirection(ad.a.west(), 3))
          case (3, "|") => Seq(AddressDirection(ad.a.north(), 0), AddressDirection(ad.a.south(), 2))
          case (3, "/") => Seq(AddressDirection(ad.a.south(), 2))
          case (3, "\\") => Seq(AddressDirection(ad.a.north(), 0))
          case (3, ".") => Seq(AddressDirection(ad.a.west(), 3))

          case _ =>
            println(ad.d)
            println(currentSymbol)
            throw new RuntimeException("resultants could not be calculated")
        }
        resultants.flatMap(_.bound(maxes._1, maxes._2))
      })

      if ((newAcc.distinct diff acc).isEmpty) newAcc.distinct
      else propagate(acMap, maxes, newAcc.distinct)
    }

    val acMap: Map[Address, Char] = lines.zipWithIndex.flatMap(row => {
      row._1.zipWithIndex.flatMap(col => {
        if (col._1 == ".".head) None
        else Some(Map(Address(row._2, col._2) -> col._1))
      })
    }).foldLeft(Map.empty[Address, Char]) {
      (mapLeft, mapRight) => mapLeft ++ mapRight
    }

    val maxes = (lines.head.length - 1, lines.length - 1)

    val a1Beams: Seq[AddressDirection] = propagate(acMap, maxes, Seq(AddressDirection(Address(0, 0), 1)))
    val answer1 = a1Beams.map(_.a).distinct.length

    val a2RowStarts: Seq[AddressDirection] = (0 to maxes._1).flatMap(row => {
      Seq(AddressDirection(Address(row, 0), 1), AddressDirection(Address(row, maxes._2), 3))
    })
    val a2ColStarts: Seq[AddressDirection] = (0 to maxes._2).flatMap(col => {
      Seq(AddressDirection(Address(0, col), 2), AddressDirection(Address(maxes._1, col), 0))
    })
    val a2Starts = a2RowStarts ++ a2ColStarts
    val a2Results: Seq[Int] = a2Starts.map(a2Start => {
      val a2Beams: Seq[AddressDirection] = propagate(acMap, maxes, Seq(a2Start))
      a2Beams.map(_.a).distinct.length
    })

    val answer2 = a2Results.max

    (answer1, answer2)

  }

  println(solveDay(day = 16, test = "test"))
  println(solveDay(day = 16))

}

case class AddressDirection(a: Address, d: Int) {
  def bound(maxX: Int, maxY: Int): Option[AddressDirection] = {
    if      (this.a.r > maxY) None
    else if (this.a.r < 0   ) None
    else if (this.a.c > maxX) None
    else if (this.a.c < 0   ) None
    else Some(this)
  }
}
