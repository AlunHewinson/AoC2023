package aoc2023

import utils.Address
import utils.utils.readDay
import scala.annotation.tailrec

object day17 extends App {
  // https://adventofcode.com/2023/day/17

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val lineSeparator: String = "\r*\n"
    val lines: Array[String] = lineSeparator.r.split(inp)

    val maxes = (lines.head.length - 1, lines.length - 1)

    val avMap: Map[Address, Int] = lines.zipWithIndex.flatMap(row => {
      row._1.zipWithIndex.map(col => {
        Map(Address(row._2, col._2) -> col._1.toString.toInt)
      })
    }).foldLeft(Map.empty[Address, Int]) {
      (mapLeft, mapRight) => mapLeft ++ mapRight
    }

    @tailrec
    def propagate(toCheck: Seq[AddressDirectionRunTotal], checked: Seq[AddressDirectionRunTotal],
                  avMap: Map[Address, Int], part2: Boolean): Seq[AddressDirectionRunTotal] = {

      val candidates = toCheck.
        flatMap(x => {
          val minStraight: Int = if (part2) 4 else 0
          val toFilter: Seq[AddressDirectionRunTotal] = Seq(x.north(avMap, minStraight), x.east(avMap, minStraight),
            x.south(avMap, minStraight), x.west(avMap, minStraight))
            .flatten
          if (part2) toFilter.filter(_.validPath(maxes._1, maxes._2, 10))
          else toFilter.filter(_.validPath(maxes._1, maxes._2, 3))
        })

      val bestADRs: Seq[AddressDirectionRunTotal] = (candidates ++ checked).
        groupBy(_.adr).
        view.
        mapValues(_.minBy(_.total)).
        toMap.
        values.
        toSeq

      if (checked.map(_.hashCode()).sorted == bestADRs.map(_.hashCode()).sorted) bestADRs //checked
      else propagate(bestADRs diff checked, bestADRs, avMap, part2)

    }

    val starting: Seq[AddressDirectionRunTotal] = Seq(
      AddressDirectionRunTotal(AddressDirectionRun(Address(0, 0), 1, 0), 0),
      AddressDirectionRunTotal(AddressDirectionRun(Address(0, 0), 2, 0), 0))

    val allPaths1: Seq[AddressDirectionRunTotal] = propagate(starting, Seq(), avMap, part2 = false)
      .filter(_.adr.a == Address(maxes._2, maxes._1))
      .groupBy(_.adr.a)
      .view
      .mapValues(_.minBy(_.total))
      .toMap
      .values
      .toSeq

    val allPaths2: Seq[AddressDirectionRunTotal] = propagate(starting, Seq(), avMap, part2 = true)
      .filter(_.adr.run >= 4)
      .filter(_.adr.a == Address(maxes._2, maxes._1))
      .groupBy(_.adr.a)
      .view
      .mapValues(_.minBy(_.total))
      .toMap
      .values
      .toSeq

    val answer1 = allPaths1.head.total
    val answer2 = allPaths2.head.total

    (answer1, answer2)

  }

  //println(solveDay(day = 17, test = "minitest"))
  println(solveDay(day = 17, test = "test"))
  println(solveDay(day = 17))

}

case class AddressDirectionRun(a: Address, dir: Int, run: Int) {
  def show(): Unit = {
    println(s"@$a,   direction $dir,   run length $run")
  }
}
case class AddressDirectionRunTotal(adr: AddressDirectionRun, total: Int) {
  def show(): Unit = {
    println(s"@${adr.a},   direction ${adr.dir},   run length ${adr.run},   total $total")
  }
  def north(avMap: Map[Address, Int], minStraight: Int): Option[AddressDirectionRunTotal] = {
    if (this.adr.dir == 2) None
    else if (this.adr.dir != 0 & this.adr.run < minStraight) None
    else Some(AddressDirectionRunTotal(AddressDirectionRun(this.adr.a.north(), 0, if (this.adr.dir == 0) this.adr.run + 1 else 1),
      this.total + avMap.getOrElse(this.adr.a.north(), 100000)))
  }
  def east(avMap: Map[Address, Int], minStraight: Int): Option[AddressDirectionRunTotal] = {
    if (this.adr.dir == 3) None
    else if (this.adr.dir != 1 & this.adr.run < minStraight) None
    else Some(AddressDirectionRunTotal(AddressDirectionRun(this.adr.a.east(), 1, if (this.adr.dir == 1) this.adr.run + 1 else 1),
      this.total + avMap.getOrElse(this.adr.a.east(), 100000)))
  }
  def south(avMap: Map[Address, Int], minStraight: Int): Option[AddressDirectionRunTotal] = {
    if (this.adr.dir == 0) None
    else if (this.adr.dir != 2 & this.adr.run < minStraight) None
    else Some(AddressDirectionRunTotal(AddressDirectionRun(this.adr.a.south(), 2, if (this.adr.dir == 2) this.adr.run + 1 else 1),
      this.total + avMap.getOrElse(this.adr.a.south(), 100000)))
  }
  def west(avMap: Map[Address, Int], minStraight: Int): Option[AddressDirectionRunTotal] = {
    if (this.adr.dir == 1) None
    else if (this.adr.dir != 3 & this.adr.run < minStraight) None
    else Some(AddressDirectionRunTotal(AddressDirectionRun(this.adr.a.west(), 3, if (this.adr.dir == 3) this.adr.run + 1 else 1),
      this.total + avMap.getOrElse(this.adr.a.west(), 100000)))
  }

  def validPath(maxX: Int, maxY: Int, maxLength: Int): Boolean = {
    if (adr.run > maxLength) false
    else if (this.adr.a.r > maxY) false
    else if (this.adr.a.r < 0) false
    else if (this.adr.a.c > maxX) false
    else if (this.adr.a.c < 0) false
    else true
  }

}
