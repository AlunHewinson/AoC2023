package aoc2015

import utils.utils.readDay

import scala.collection.immutable.Seq

object day02 extends App {
  // https://adventofcode.com/2015/day/2

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2015)
    val lineSeparator: String = "(\r*\n)"
    val lines: Seq[String] = lineSeparator.r.split(inp)
    val lineDims: Seq[Seq[Int]] = lines.map(l => "x".r.split(l).map(_.toInt))

    def comb2[T](x: Seq[(T, Int)]): Seq[(T, T)] = {
      x.flatMap(e1 => x.flatMap(e2 => {
        if (e1._2 == e2._2) None else Some((e1._1, e2._1))
      }))
    }

    def paper(dims: Seq[Int]): Int = {
      val cover = comb2(dims.zipWithIndex).map(e => e._1 * e._2)
      cover.sum + cover.min
    }

    def ribbon(dims: Seq[Int]): Int = {
      dims.sorted.take(2).sum * 2 + dims.product
    }

    val answer1 = lineDims.map(paper).sum
    val answer2 = lineDims.map(ribbon).sum

    (answer1, answer2)

  }

  println(solveDay(day = 2, test = "test"))
  println(solveDay(day = 2))

}
