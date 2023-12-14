package aoc2015

import utils.Address
import utils.utils.readDay

object day03 extends App {
  // https://adventofcode.com/2015/day/3

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2015)

    def deliver(address: Address, instructions: String, acc: Map[Address, Int]): Map[Address, Int] = {

      val n = "^".head
      val e = ">".head
      val s = "v".head
      val w = "<".head

      if (instructions.isEmpty) acc
      else {
        val i1 = instructions.head
        val newAddress = i1 match {
          case q if q==n => address.north()
          case q if q==e => address.east()
          case q if q==s => address.south()
          case q if q==w => address.west()
        }
        val oldValue = acc.getOrElse(newAddress, -1)
        val newAcc: Map[Address, Int] = acc + (newAddress -> (oldValue + 1))
        deliver(newAddress, instructions.tail, newAcc)
      }
    }

    val answer1 = deliver(Address(0, 0), inp, Map(Address(0, 0) -> 1)).size

    val inpSanta = inp.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).mkString("")
    val inpRobot = inp.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).mkString("")

    val santa = deliver(Address(0, 0), inpSanta, Map(Address(0, 0) -> 1))
    val robot = deliver(Address(0, 0), inpRobot, Map(Address(0, 0) -> 1))

    val answer2 = (santa ++ robot).size

    (answer1, answer2)

  }

  println(solveDay(day = 3, test = "test"))
  println(solveDay(day = 3))

}
