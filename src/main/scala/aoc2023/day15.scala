package aoc2023

import utils.utils.readDay

import scala.annotation.tailrec
import scala.collection.mutable

object day15 extends App {
  // https://adventofcode.com/2023/day/15

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val inpSeparator: String = ","
    val inputs: Seq[String] = inpSeparator.r.split(inp)

    @tailrec
    def hoHoHash(x: Seq[Char], acc: Int = 0): Int = {
      if (x.isEmpty) acc
      else hoHoHash(x.tail, ((acc + x.head.toInt) * 17) % 256)
    }

    @tailrec
    def hoHoHashmap(x: Seq[String],
                    h: mutable.LinkedHashMap[Int, mutable.LinkedHashMap[String, Int]] = mutable.LinkedHashMap()
                   ): mutable.LinkedHashMap[Int, mutable.LinkedHashMap[String, Int]] = {

      if (x.isEmpty) h
      else {
        x.head match {
          case s"$label-" => {
            val box = hoHoHash(label)
            val innerHashMap: mutable.LinkedHashMap[String, Int] = h.getOrElse(box, mutable.LinkedHashMap()) //h(box)
            innerHashMap.remove(label)
            h ++= Map(box -> innerHashMap)
          }
          case s"$label=$value" => {
            val box = hoHoHash(label)
            val innerHashMap: mutable.LinkedHashMap[String, Int] = h.getOrElse(box, mutable.LinkedHashMap()) //h(box)
            innerHashMap ++= mutable.LinkedHashMap(label -> value.toInt)
            h ++= mutable.LinkedHashMap(box -> innerHashMap)
          }
        }
        hoHoHashmap(x.tail, h)
      }
    }

    val answer1 = inputs.map(i => hoHoHash(i.toSeq)).sum

    val answer2 = hoHoHashmap(inputs).map(o => {
      val boxScore = o._1 + 1
      o._2.zipWithIndex.map(i => {
        val slotScore = i._2 + 1
        val focalLength = i._1._2
        boxScore * slotScore * focalLength
      }).sum
    }).sum

    (answer1, answer2)

  }

  println(solveDay(day = 15, test = "test"))
  println(solveDay(day = 15))

}
