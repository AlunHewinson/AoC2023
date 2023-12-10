package aoc2023

import utils.utils.readDay
import scala.annotation.tailrec
import math.ceil

object day10 extends App {
  // https://adventofcode.com/2023/day/10

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val rowSeparator: String = "\r*\n"
    val rows: Array[String] = rowSeparator.r.split(inp)

    val grid = Grid(cells = rows.zipWithIndex.flatMap(r => {
      val rowNumber = r._2
      r._1.zipWithIndex.map(c => {
        val colNumber = c._2
        c._1 match {
          case s if s=="S".head => Cell(Address(rowNumber, colNumber), "F".head, 0) // F by inspection in my personal input
          case _                => Cell(Address(rowNumber, colNumber), c._1, -1)
        }
      })
    }))

    val finishedGrid = grid.stepMany().calculateInternal()

    val answer1 = finishedGrid.getFarthest()
    val answer2 = finishedGrid.internal.count(x => x)

    (answer1, answer2)

  }

  println(solveDay(10, test = true))
  println(solveDay(10))

}

case class Address(r: Int, c: Int) {
  def east(): Address  = Address(this.r, this.c + 1)
  def south(): Address = Address(this.r + 1, this.c)
  def west(): Address  = Address(this.r, this.c - 1)
  def north(): Address = Address(this.r - 1, this.c)
}

case class Cell(address: Address, component: Char, milestone: Int) {
  def show(): Unit = {
    println(s"Cell with address ${this.address}, component ${this.component}, milestone ${this.milestone}")
  }
  def findNeighbours(): Seq[Address] = {
    //println(this.component)
    this.component match {
      case x if x=="F".head => Seq(this.address.east(), this.address.south())
      case x if x=="7".head => Seq(this.address.west(), this.address.south())
      case x if x=="J".head => Seq(this.address.west(), this.address.north())
      case x if x=="L".head => Seq(this.address.east(), this.address.north())
      case x if x=="|".head => Seq(this.address.south(), this.address.north())
      case x if x=="-".head => Seq(this.address.east(), this.address.west())
      case x if x=="S".head => Seq(this.address.east(), this.address.south()) // by inspection in my personal input
    }
  }
  // Function to calculate the winding number for a given point and a sequence of boundary points
  def calculateWindingNumber(grid: Grid): Int = {
    val boundaryTuple: Seq[(Int, Address)] = grid.cells.flatMap(c => {
      c.milestone match {
        case -1 => None
        case _  => Some((c.milestone, c.address))
      }
    })
    val boundaryTupleSorted = boundaryTuple.sortBy(_._1)
    val boundary: Seq[Address] = boundaryTupleSorted.map(bt => bt._2)
    if (boundary.contains(this.address)) 0
    else {

      val numPoints = boundary.length
      var windingNumber = 0

      for (i <- 0 until numPoints) {
        val currentPoint = boundary(i)
        val nextPoint = boundary((i + 1) % numPoints)

        if (currentPoint.r <= this.address.r) {
          if (nextPoint.r > this.address.r && isLeft(currentPoint, nextPoint, this.address)) {
            windingNumber += 1
          }
        } else {
          if (nextPoint.r <= this.address.r && isLeft(nextPoint, currentPoint, this.address)) {
            windingNumber -= 1
          }
        }
      }
      windingNumber
    }
  }
  // Helper method to determine if the point is to the left of the line formed by two points
  private def isLeft(p1: Address, p2: Address, p: Address): Boolean = {
    (p2.c - p1.c) * (p.r - p1.r) - (p.c - p1.c) * (p2.r - p1.r) > 0
  }
}

case class Grid(cells: Seq[Cell], internal: Seq[Boolean] = Seq()) {
  def summarise(): Unit = {
    this.findMaximumMilestoneCells().foreach(_.show())
  }
  def getFarthest(): Int = {
    ceil(this.findMaximumMilestoneCells().head.milestone / 2.0).toInt
  }
  def show(): Unit = {
    val maxRow = this.cells.map(_.address.r).max
    val maxCol = this.cells.map(_.address.c).max
    val lines: Seq[String] = (0 to maxRow).map(r => {
      (0 to maxCol).map(c => {
        println(s"r: $r  c: $c")
        val milestone = getCell(Address(r, c)).milestone
        milestone match {
          case 0  => "S".head
          case -1 => ".".head
          case _  => "*".head
        }
      }).mkString("")
    })
    lines.foreach(println)
  }
  private def getCell(address: Address): Cell = {
    cells.filter(c => address == c.address).head
  }
  private def findMaximumMilestoneCells(): Seq[Cell] = {
    val maxMilestone: Int = this.cells.map(c => c.milestone).max
    this.cells.filter(c => c.milestone==maxMilestone)
  }
  private def eliminateCell(address: Address): Grid = {
    Grid(this.cells.filterNot(c => address == c.address))
  }
  private def setCellMilestone(cell: Cell, milestone: Int, dis: Grid): Grid = {
    Grid(dis.eliminateCell(cell.address).cells :+ cell.copy(milestone = milestone))
  }
  @tailrec
  private def setMultipleCellMilestones(cells: Seq[Cell], milestone: Int, dis: Grid = this): Grid = {
    if (cells.length == 1) setCellMilestone(cells.head, milestone, dis)
    else setMultipleCellMilestones(cells.tail, milestone, dis)
  }
  private def stepOnce(): Grid = {
    val starts: Seq[Cell] = this.findMaximumMilestoneCells()
    val milestone: Int = starts.map(_.milestone + 1).max
    val nexts: Seq[Cell] = starts.flatMap(_.findNeighbours()).map(getCell).filter(n => n.milestone < 0)
    if (nexts.nonEmpty) setMultipleCellMilestones(nexts, milestone)
    else this
  }
  @tailrec
  final def stepMany(): Grid = {
    val newGrid = this.stepOnce()
    if (newGrid == this) newGrid
    else newGrid.stepMany()
  }
  def calculateInternal(): Grid = {
    val internals: Seq[Boolean] = this.cells.map(_.calculateWindingNumber(this) != 0)
    this.copy(internal = internals)
  }
}
