package aoc2023

import utils.utils.readDay
import scala.annotation.tailrec

object day22 extends App {
  // https://adventofcode.com/2023/day/22

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val lineSeparator: String = "\r*\n"
    val lines: Seq[String] = lineSeparator.r.split(inp)

    val bricks: Seq[Brick] = lines.zipWithIndex.map(l => {
      val ends: Array[String] = l._1.split("~")
      Brick(l._2 + 1, ends)
    })

    def overlap(brick: Brick, stack: Seq[Brick]): Boolean = {
      val overlap = stack.exists(s => {
        (s.x.contains(brick.x.start) || brick.x.contains(s.x.start)) &&
          (s.y.contains(brick.y.start) || brick.y.contains(s.y.start)) &&
          (s.z.contains(brick.z.start) || brick.z.contains(s.z.start))
      })
      val grounded = brick.z.start < 1
      overlap || grounded
    }

    def whichSupport(brick: Brick, stack: Seq[Brick]): Seq[Int] = {
      val downOne = brick.copy(z = brick.z.start - 1 to brick.z.end - 1)
      if (downOne.z.start < 1) Seq(0)
      else {
        val supportedBy: Seq[Int] = stack.flatMap(s => {
          val overlap: Boolean = (s.x.contains(downOne.x.start) || downOne.x.contains(s.x.start)) &&
            (s.y.contains(downOne.y.start) || downOne.y.contains(s.y.start)) &&
            (s.z.contains(downOne.z.start) || downOne.z.contains(s.z.start))
          if (overlap) Some(s.id) else None
        })
        if (supportedBy.isEmpty) Seq()
        else supportedBy.filterNot(_ == brick.id)
      }
    }

    @tailrec
    def compress(loose: Seq[Brick], tight: Seq[Brick]): Seq[Brick] = {

      val looseSorted: Seq[Brick] = loose.sortBy(_.z.start)

      @tailrec
      def fall(dette: Brick): Brick = {
        val downOne = dette.copy(z = dette.z.start - 1 to dette.z.end - 1)
        if (overlap(downOne, tight)) dette
        else fall(downOne)
      }

      if (looseSorted.isEmpty) tight
      else {
        val fallen: Brick = fall(looseSorted.head)
        val newTight = tight :+ fallen
        compress(looseSorted.tail, newTight)
      }

    }
    val jenga: Seq[Brick] = compress(bricks, Seq()).sortBy(b => b.z.min)

    val supportingJenga = jenga.map(j => {
      j.copy(supportedBy = whichSupport(j, jenga))
    })
    val a1Help: Seq[Int] = supportingJenga.map(_.id) diff supportingJenga.filter(_.supportedBy.length == 1).flatMap(_.supportedBy)

    val answer1 = a1Help.length

    def resolveLoad3(eliminate: Int, pile: Seq[Brick] = supportingJenga) = {
      val jengaed = pile.filterNot(_.id == eliminate)
      val collapsed = compress(jengaed, Seq())
      jengaed.map(j => {
        collapsed.filter(_.id == j.id).head.z.start != j.z.start
      }).count(_ == true)
    }

    val a2Targets: Seq[Int] = supportingJenga.filter(_.supportedBy.length == 1).flatMap(_.supportedBy).distinct diff Seq(0)
    val a2Help = supportingJenga
      .filter(a2Targets contains _.id)
      .map(x => resolveLoad3(x.id))

    val answer2 = a2Help.sum

    (answer1, answer2)

  }

  println(solveDay(day = 22, test = "test")) // 499
  println(solveDay(day = 22)) // 95059

}

case class Brick(id: Int, x: Range, y: Range, z: Range, supportedBy: Seq[Int] = Seq()) {
  def show(): Unit = {
    println(s"${this.id}  ==>  ${this.x}  ,  ${this.y}  ,  ${this.z}     supported by: ${this.supportedBy}")
  }
}
object Brick {
  def apply(id: Int, ends: Array[String]): Brick = {
    val b = ends.map(e => {
      e.split(",").map(_.toInt)
    })
    Brick(id, b.head.head to b(1).head, b.head(1) to b(1)(1), b.head(2) to b(1)(2))
  }
}