package aoc2023

import utils.utils.readDay
import utils.Address
import java.time.LocalTime
import scala.annotation.tailrec

object day14 extends App {
  // https://adventofcode.com/2023/day/14

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val lineSeparator: String = "(\r*\n)"
    val lines: Seq[String] = lineSeparator.r.split(inp)
    val fixedRock = "#".head
    val mobileRock = "O".head
    val space = ".".head

    def parseRocks(lines: Seq[String]): Landscape = {
      Landscape(rocks = lines.zipWithIndex.flatMap(l => {
        l._1.zipWithIndex.flatMap(c => {
          c._1 match {
            case rock if rock == fixedRock => Some(Rock(mobile = false, space = false, Address(l._2, c._2), rock))
            case rock if rock == mobileRock => Some(Rock(mobile = true, space = false, Address(l._2, c._2), rock))
            case rock if rock == space => Some(Rock(mobile = true, space = true, Address(l._2, c._2), rock))
            case _ => None
          }
        })
      }))
    }

    val landscape: Landscape = parseRocks(lines)

    val answer1 = landscape.tiltNorth(parseRocks).loadNorth()
    val answer2 = landscape.cycle(parseRocks, nn = 1000000000).loadNorth()

    (answer1, answer2)

  }

  println(solveDay(14, test = true))
  println(solveDay(14))

}

case class Rock(mobile: Boolean, space: Boolean, address: Address, character: Char)
case class Landscape(rocks: Seq[Rock]) {
  private val maxRow: Int = this.rocks.map(r => r.address.r).max
  private def makeString(): Seq[String] = {
    val grouped = this.rocks.groupBy(x => x.address.r)
    (0 to this.maxRow).map(n => {
      grouped(n).sortBy(x => x.address.c).map(rock => rock.character).mkString("")
    })
  }
  def show(): Unit = {
    val grouped = this.rocks.groupBy(x => x.address.r)
    println((0 to this.maxRow).map(n => {
      val qqq: String = grouped(n).sortBy(x => x.address.c).map(rock => rock.character).mkString("")
      qqq
    }).mkString("\n"))
  }
  def tiltNorth(parser: Seq[String] => Landscape): Landscape = {

    val skcor = this.makeString().transpose.map(x => {
      x.
        mkString("").
        replace("#", "#Q").
        split("#").
        map(_.sorted.reverse).
        mkString("#").
        replace("Q", "")
    })
    parser(skcor.transpose.map(_.mkString("\n")))
  }
  private def tiltSouth(parser: Seq[String] => Landscape): Landscape = {
    val skcor = this.makeString().transpose.map(x => {
      x.
        mkString("").
        replace("#", "#Q").
        split("#").
        map(_.sorted).
        mkString("#").
        replace("Q", "")
    })
    parser(skcor.transpose.map(_.mkString("\n")))
  }
  private def tiltWest(parser: Seq[String] => Landscape): Landscape = {
    val skcor = this.makeString().map(x => {
      x.
        mkString("").
        replace("#", "#Q").
        split("#").
        map(_.sorted.reverse).
        mkString("#").
        replace("Q", "")
    })
    parser(skcor.map(_.mkString("\n")))
  }
  private def tiltEast(parser: Seq[String] => Landscape): Landscape = {
    val skcor = this.makeString().map(x => {
      x.
        mkString("").
        replace("#", "#Q").
        split("#").
        map(_.sorted).
        mkString("#").
        replace("Q", "")
    })
    parser(skcor.map(_.mkString("\n")))
  }

  @tailrec
  final def cycle(parser: Seq[String] => Landscape, nn: Int, n: Int = -1, states: Map[Int, Int] = Map()): Landscape = {
    if (n == -1) {
      this.cycle(parser, nn, nn-1, states)
    } else if (n == 0) {
      this
    } else {
      val oneCycle = this.tiltNorth(parser).tiltWest(parser).tiltSouth(parser).tiltEast(parser)
      val state = oneCycle.hashCode()

      val newNWipeStates = if (states.isDefinedAt(state)) {
        (n % (states(state) - n), true)
      } else {
        (n - 1, false)
      }
      val newN = newNWipeStates._1
      val newStates = if (newNWipeStates._2) Map(-1 -> -1) else states + (state -> n)
      oneCycle.cycle(parser, nn, newN, newStates)
    }
  }
  def loadNorth(): Int = {
    this.rocks.filter(rock => rock.character=="O".head).map(rock => this.maxRow + 1 - rock.address.r).sum
  }
}
