package aoc2023

import utils.utils.readDay

import scala.annotation.tailrec
import scala.collection.immutable.Seq

object day13 extends App {
  // https://adventofcode.com/2023/day/13

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val gridSeparator: String = "(\r*\n){2,}"
    val grids: Seq[String] = gridSeparator.r.split(inp)

    def findReflectionNumber(grid: String, invalidAnswer: Double = -1.0): Int = {
      val lines: Array[String] = "\r*\n".r.split(grid)
      val cols: Array[String] = lines.map(l => l.map(_.toString).toArray).transpose.map(_.mkString(""))

      val lCombs = lines.zipWithIndex.combinations(2)
      val cCombs: Iterator[Array[(String, Int)]] = cols.zipWithIndex.combinations(2)

      val lGroups: Map[Double, Boolean] = lCombs.map(lc => {
        ((lc.head._2 + lc.last._2 + 1) / 2.0, lc.head._1 == lc.last._1)
      }).toSeq.groupMapReduce(_._1)(_._2)(_ && _)
      val cGroups: Map[Double, Boolean] = cCombs.map(cc => {
        ((cc.head._2 + cc.last._2 + 1) / 2.0, cc.head._1 == cc.last._1)
      }).toSeq.groupMapReduce(_._1)(_._2)(_ && _)

      val betweenLines = lGroups.filter(x => x._1 % 1 == 0).map(x => (x._1 * 100, x._2))
      val betweenCols =  cGroups.filter(x => x._1 % 1 == 0)

      Seq(betweenLines, betweenCols).
        flatten.
        find(x => x._1 != invalidAnswer & x._2).
        getOrElse((0.0, false)).
        _1.
        toInt
    }

    val answer1 = grids.map(x => findReflectionNumber(x)).sum

    @tailrec
    def smudge(grid: String, originalReflectionNumber: Int = 0, n: Int = 0): Int = {

      val cachedOriginalReflectionNumber = if (originalReflectionNumber == 0) {
        findReflectionNumber(grid)
      } else {
        originalReflectionNumber
      }

      val smudgeChar = grid(n)
      val rn = smudgeChar match {
        case x if x=="#".head => findReflectionNumber(grid.updated(n, ".".head), cachedOriginalReflectionNumber)
        case x if x==".".head => findReflectionNumber(grid.updated(n, "#".head), cachedOriginalReflectionNumber)
        case _                => 0
      }

      if (rn > 0) rn
      else smudge(grid, cachedOriginalReflectionNumber, n + 1)

    }

    val answer2 = grids.map(x => smudge(x)).sum

    (answer1, answer2)

  }

  println(solveDay(13, test = true))
  println(solveDay(13))

}
