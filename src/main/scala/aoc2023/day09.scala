package aoc2023

import utils.utils.readDay

import scala.annotation.tailrec

object day09 extends App {
  // https://adventofcode.com/2023/day/9

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val lineSeparator: String = "\r*\n"
    val lines: Array[String] = lineSeparator.r.split(inp)
    val numberSeparator: String = " "
    val lineNumbers = lines.map(l => numberSeparator.r.split(l).map(_.toInt))
    //lineNumbers.map(_.mkString(", ")).foreach(println)

    def getNext(x: Array[Int]): Int = {
      //println(s"x: ${x.mkString(", ")}")
      val differences: Array[Int] = x.sliding(2).collect { case Array(a, b) => b - a }.toArray
      if (differences.forall(_ == 0)) {
        //println(s"returning ${x.last}")
        x.last
      }
      else {
        val yo = x.last + getNext(differences)
        //println(s"returning $yo")
        yo
      }
    }

    val nexts: Array[Int] = lineNumbers.map(getNext)
    //nexts.foreach(println)

    val answer1 = nexts.sum

    def getPrevious(x: Array[Int]): Int = {
      //println(s"x: ${x.mkString(", ")}")
      val differences: Array[Int] = x.sliding(2).collect { case Array(a, b) => b - a }.toArray
      if (differences.forall(_ == 0)) {
        //println(s"returning ${x.last}")
        x.head
      }
      else {
        val yo = x.head - getPrevious(differences)
        yo
      }
    }

    val previous: Array[Int] = lineNumbers.map(getPrevious)
    val answer2 = previous.sum

    (answer1, answer2)

  }

  println(solveDay(9, test = true))
  println(solveDay(9))

}
