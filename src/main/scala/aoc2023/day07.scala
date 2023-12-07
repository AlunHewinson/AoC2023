package aoc2023

import utils.utils.readDay

import scala.collection.MapView
import scala.collection.immutable.{Map, Seq}
import scala.math.Numeric.IntIsIntegral.sign
import scala.math.Ordering.Implicits.seqOrdering

object day07 extends App {
  // https://adventofcode.com/2023/day/7

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val lineSeparator: String = "\r*\n"
    val lines: Array[String] = lineSeparator.r.split(inp)

    val cardOrder: Map[String, Int] = Map("2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9,
      "T" -> 10, "J" -> 11, "Q" -> 12, "K" -> 13, "A" -> 14)

    val handBids: Array[Seq[Int]] = lines.map(l => {
      val hb: Array[String] = " ".r.split(l)
      val bid = hb.tail.head.toInt
      val cards: Seq[Int] = for (c <- hb.head) yield cardOrder(c.toString)
      val power: Int = 5 - cards.distinct.length
      val effort: Int = cards.groupBy(identity).view.mapValues(_.size).values.max
      Seq(power, effort, cards.head, cards(1), cards(2), cards(3), cards(4), bid)
    })
    val sortedHands = handBids.sortBy(_.zipWithIndex)
    val withRank: Array[(Seq[Int], Int)] = sortedHands.zipWithIndex.map(x => (x._1, x._2+1))
    val winnings: Array[Int] = withRank.map(x => x._1.last * x._2)

    val answer1 = winnings.sum

    val jokerOrder: Map[String, Int] = Map("2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9,
      "T" -> 10, "J" -> 1, "Q" -> 12, "K" -> 13, "A" -> 14)

    val jokerBids: Array[Seq[Int]] = lines.map(l => {
      val hb: Array[String] = " ".r.split(l)
      val bid = hb.tail.head.toInt
      val cards: Seq[Int] = for (c <- hb.head) yield jokerOrder(c.toString)
      val jCount = cards.count(_ == 1)
      val jokerPowerAdjustment = if (cards.max == 1) 0 else sign(jCount)
      val power: Int = 5 - cards.distinct.length + jokerPowerAdjustment
      val effort: Int = (cards.filterNot(_ == 1).groupBy(identity).view.mapValues(_.size).values.toList :+ 0).max + jCount
      Seq(power, effort, cards.head, cards(1), cards(2), cards(3), cards(4), bid)
    })
    val sortedJokerHands = jokerBids.sortBy(_.zipWithIndex)
    sortedJokerHands.foreach(println)
    val withRankJoker: Array[(Seq[Int], Int)] = sortedJokerHands.zipWithIndex.map(x => (x._1, x._2 + 1))
    val jokerWinnings: Array[Int] = withRankJoker.map(x => x._1.last * x._2)

    val answer2 = jokerWinnings.sum
    (answer1, answer2)

  }

  println(solveDay(7, test = true))
  println(solveDay(7))

}
