package aoc2023

import utils.utils.readDay

import math.abs
import scala.collection.immutable.Seq

object day03 extends App {
  // https://adventofcode.com/2023/day/3

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {

    val inp: String = readDay(day, test, year = 2023)
    val rowSeparator: String = "\r*\n"
    val rows = rowSeparator.r.split(inp)

    val symbolRegex = "[^0-9.]".r
    val numberRegex = "[0-9]+".r
    val gearRegex = "\\*".r

    def rangify(start: Int, end: Int): Range.Exclusive = {
      Range(start - 1, end + 1)
    }

    val parts: Seq[Part] = rows.zipWithIndex.flatMap(rowIndex => {
      numberRegex.findAllMatchIn(rowIndex._1).map(n => {
        Part(n.matched.toInt, rowIndex._2, rangify(n.start, n.end))
      }).toList
    }).toSeq

    val symbols: Seq[Symbol] = rows.zipWithIndex.flatMap(rowIndex => {
      symbolRegex.findAllMatchIn(rowIndex._1).map(n => {
        Symbol(n.matched, rowIndex._2, n.start)
      }).toList
    }).toSeq

    val gears: Seq[Gear] = rows.zipWithIndex.flatMap(rowIndex => {
      gearRegex.findAllMatchIn(rowIndex._1).map(n => {
        Gear(n.matched, rowIndex._2, n.start)
      }).toList
    }).toSeq

    val partConnections = parts.map(part => part.isConnectedToASymbol(symbols))
    val gearConnections = gears.map(gear => gear.getGearRatio(parts))

    val part1Answer = partConnections.zip(parts).map(x => {
      if (x._1) x._2.partId else 0
    }).sum
    val part2Answer = gearConnections.sum

    (part1Answer, part2Answer)
  }

  println(solveDay(3, test = true))
  println(solveDay(3))

}

case class Part(partId: Int, row: Int, columns: Range.Exclusive) {
  def isConnectedToASymbol(symbols: Seq[Symbol]): Boolean = {
    symbols.exists(s => {
      (abs(s.row - this.row) < 2) & this.columns.contains(s.column)
    })
  }
}

case class Symbol(symbol: String, row: Int, column: Int)

case class Gear(symbol: String, row: Int, column: Int) {
  def getGearRatio(parts: Seq[Part]): Int = {
    val connectedParts = parts.flatMap(p => {
      if ((abs(p.row - this.row) < 2) & p.columns.contains(this.column)) {
        Some(p)
      } else None
    })
    if (connectedParts.length == 2) {
      connectedParts.head.partId * connectedParts.tail.head.partId
    } else 0
  }
}
