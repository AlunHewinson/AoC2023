package aoc2023

import utils.BigAddress
import utils.utils.readDay
import scala.annotation.tailrec

object day18 extends App {
  // https://adventofcode.com/2023/day/18

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val lineSeparator: String = "\r*\n"
    val lines: Array[String] = lineSeparator.r.split(inp)

    val digInstructions1 = lines.map(l => (l.head.toString, " [0-9]+ ".r.findFirstIn(l).getOrElse("0").trim.toInt))
    val digInstructions2 = lines.map(l => {
      (l.takeRight(2).head.toString match {
        case "0" => "R"
        case "1" => "D"
        case "2" => "L"
        case "3" => "U"
      }, Integer.parseInt("[0-9a-f]{5}".r.findFirstIn(l).getOrElse("000000"), 16))
    })

    @tailrec
    def hop(directionLength: Seq[(String, Int)], acc: Seq[BigAddress]): Seq[BigAddress] = {
      if (directionLength.isEmpty) acc
      else {
        hop(directionLength.tail, acc.appended(directionLength.head match {
          case ("U", n) => acc.last.north(n)
          case ("R", n) => acc.last.east(n)
          case ("D", n) => acc.last.south(n)
          case ("L", n) => acc.last.west(n)
        }))
      }
    }
    val nodes1: Seq[BigAddress] = hop(digInstructions1, Seq(BigAddress(0, 0)))
    val nodes2: Seq[BigAddress] = hop(digInstructions2, Seq(BigAddress(0, 0)))

    def shoelace(a: Seq[BigAddress]): BigInt = {
      val l = a.length

      val areaLengths = (0 until l-1).map(n => {
        (a(n).r * a(n+1).c - a(n+1).r * a(n).c, (a(n).r - a(n+1).r + a(n).c - a(n+1).c).abs)
      })

      val areaWithin = areaLengths.map(_._1).sum.abs / 2
      val lengths_along = areaLengths.map(_._2).sum.abs / 2 + 1

      areaWithin + lengths_along

    }

    val answer1 = shoelace(nodes1)
    val answer2 = shoelace(nodes2)

    (answer1, answer2)

  }

  println(solveDay(day = 18, test = "test"))
  println(solveDay(day = 18))

}
