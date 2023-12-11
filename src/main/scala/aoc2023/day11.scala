package aoc2023

import utils.utils.readDay
import utils.Address

import scala.collection.immutable.Seq

object day11  extends App {
  // https://adventofcode.com/2023/day/11

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val rowSeparator: String = "\r*\n"
    val rows: Seq[String] = rowSeparator.r.split(inp)
    val universe: Universe = Universe(rows)

    val galaxy = "#".head

    val galaxies: Seq[Address] = universe.grid.zipWithIndex.flatMap(r => {
      r._1.zipWithIndex.flatMap(c => {
        if (c._1 == galaxy) Some(Address(r._2, c._2))
        else None
      })
    })

    val expansionFactor: BigInt = BigInt(1)
    val expansionRows = universe.detectEmptyRows().map(r => (r, expansionFactor))
    val expansionColumns = universe.detectEmptyColumns().map(c => (c, expansionFactor))

    val distances: Seq[BigInt] = galaxies.combinations(2).map(x => {
      x.head.expandedManhattenDistance(x.last, expansionRows, expansionColumns)
    }).toSeq

    val answer1 = distances.sum

    val bigExpansionFactor: BigInt = BigInt(999999)
    val bigExpansionRows = universe.detectEmptyRows().map(r => (r, bigExpansionFactor))
    val bigExpansionColumns = universe.detectEmptyColumns().map(c => (c, bigExpansionFactor))

    val bigDistances: Seq[BigInt] = galaxies.combinations(2).map(x => {
      x.head.expandedManhattenDistance(x.last, bigExpansionRows, bigExpansionColumns)
    }).toSeq

    val answer2: BigInt = bigDistances.sum

    (answer1, answer2)

  }

  println(solveDay(11, test = true))
  println(solveDay(11))

}

case class Universe(grid: Seq[String]) {
  private val space = ".".head

  def detectEmptyRows(): Seq[Int] = {
    this.grid.zipWithIndex.flatMap(r => {
      if (r._1.forall(_ == space)) Some(r._2)
      else None
    })
  }

  def detectEmptyColumns(): Seq[Int] = {
    this.grid.transpose.zipWithIndex.flatMap(c => {
      if (c._1.forall(_ == space)) Some(c._2)
      else None
    })
  }
}