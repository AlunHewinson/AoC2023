package aoc2023

import utils.utils.readDay

import scala.annotation.tailrec
import scala.math.pow

object day04 extends App {
  // https://adventofcode.com/2023/day/4

  def solveDay(day: Int, test: Boolean = false): (Int, Int) = {

    val inp: String = readDay(day, test, year = 2023)
    val cardSeparator: String = "\r*\n"
    val cards: Array[String] = cardSeparator.r.split(inp)

    val idSeparator: String = ": +"
    val winTries: Array[String] = cards.map(card => idSeparator.r.split(card).tail.head)

    val lineSeparator: String = " +\\| +"
    val separatedLines: Array[(String, String)] = winTries.map(winTry => {
      val separated = lineSeparator.r.split(winTry)
      (separated.head, separated.tail.head)
    })

    val separatedNumbers: Array[(Array[Int], Array[Int])] = separatedLines.map(lines => {
      val winners = " +".r.split(lines._1).map(_.toInt)
      val picks = " +".r.split(lines._2)
      (winners, picks.map(_.toInt))
    })

    val scoredCards: Array[Int] = separatedNumbers.map(card => {
      pow(2, card._2.map(pick => card._1.contains(pick)).count(_ == true) - 1).floor.toInt
    })
    val winningPicks: Array[Int] = separatedNumbers.map(card => {
      card._2.map(pick => card._1.contains(pick)).count(_ == true) //+ 1
    })

    @tailrec
    def countCards(n: Int, acc: Seq[Int]): Seq[Int] = {
      if (n < 0) {
        acc
      } else {
        val wins: Int = winningPicks(n)
        val cardsWon = acc.slice(n + 1, n + wins + 1)
        val new_acc: Seq[Int] = acc.updated(n, cardsWon.sum + 1)
        countCards(n - 1, new_acc)
      }
    }

    val answer1 = scoredCards.sum
    val answer2 = countCards(winningPicks.length - 1, winningPicks.map(_ => 1)).sum

    (answer1, answer2)

  }

  println(solveDay(4, test = true))
  println(solveDay(4))

}
