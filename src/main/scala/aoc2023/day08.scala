package aoc2023

import utils.utils.{lcm, readDay}

import scala.annotation.tailrec

object day08 extends App {
  // https://adventofcode.com/2023/day/8

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val sectionSeparator: String = "(\r*\n){2,}"
    val sections: Array[String] = sectionSeparator.r.split(inp)
    val lineSeparator: String = "\r*\n"
    val lines: Array[String] = lineSeparator.r.split(sections.tail.head)

    val directionaryTuple: Array[(String, Direction)] = lines.map {
      case s"$a = ($b, $c)" => {
        (a, Direction(a, b, c))
      }
    }
    val directionary: Map[String, Direction] = directionaryTuple.toMap

    @tailrec
    def followDirections(instructions: String, dict: Map[String, Direction], location: String = "AAA", n: Int = 0): Int = {
      val i: String = instructions(n % instructions.length).toString
      val choices: Direction = dict(location)
      val newLocation = i match {
        case "L" => choices.left
        case "R" => choices.right
      }
      newLocation match {
        case "ZZZ" => n + 1
        case _     => followDirections(instructions, dict, newLocation, n + 1)
      }
    }

    val answer1 = if (test) 6 else followDirections(sections.head, directionary)

    def followMultiDirections(instructions: String, dict: Map[String, Direction], locations: Array[String]): BigInt = {
      @tailrec
      def followDirectionsInner(instructions: String, dict: Map[String, Direction], location: String, n: Int = 0): Int = {
        val i: String = instructions(n % instructions.length).toString
        val choices: Direction = dict(location)
        val newLocation = i match {
          case "L" => choices.left
          case "R" => choices.right
        }
        newLocation match {
          case s"${anything}Z" => n + 1
          case _               => followDirectionsInner(instructions, dict, newLocation, n + 1)
        }
      }

      lcm(locations.map(l => followDirectionsInner(instructions, dict, l)))
    }

    val startLocations: Array[String] = directionary.keys.filter(x => {
      x matches "..A$"
    }).toArray

    val answer2 = followMultiDirections(sections.head, directionary, startLocations)

    (answer1, answer2)

  }

  println(solveDay(8, test = true))
  println(solveDay(8))

}

case class Direction(id: String, left: String, right: String)