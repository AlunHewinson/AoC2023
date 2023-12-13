package aoc2023

import utils.utils.readDay
import scala.collection.immutable.Seq

object day12 extends App {
  // https://adventofcode.com/2023/day/12

  def solveDay(day: Int, test: Boolean = false): Unit = { //(BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val rowSeparator: String = "\r*\n"
    val lines: Seq[String] = rowSeparator.r.split(inp)

    var memo: Map[(Int, Int, Int), BigInt] = Map()

    def process(mask: String, blocks: List[Int], characterN: Int = 0, blockN: Int = 0, current: Int = 0): BigInt = {
      val key = (characterN, blockN, current)
      if (memo.contains(key)) memo(key)                                           // if we've already processed this
      else if (characterN == mask.length) {                                       // at the end of the mask
        if ((blockN == blocks.length) && (current == 0)) 1                        // no blocks left
        else if ((blockN == blocks.length - 1) && (blocks(blockN) == current)) 1  // at the end of the last block
        else 0                                                                    // blocks left; no space for them
      } else {
        var ans: BigInt = 0
        for (c <- List('.', '#')) {
          if (mask(characterN) == c || mask(characterN) == '?') {
            if (c == '.' && current == 0) {
              ans += process(mask, blocks, characterN + 1, blockN)
            } else if (c == '.' && current > 0 && blockN < blocks.length && blocks(blockN) == current) {
              ans += process(mask, blocks, characterN + 1, blockN + 1)
            } else if (c == '#') {
              ans += process(mask, blocks, characterN + 1, blockN, current + 1)
            }
          }
        }
        memo += (key -> ans)
        ans
      }
    }

    for (part2 <- List(false, true)) {
      var ans: BigInt = 0
      for (line <- lines) {
        var Array(mask, blocks) = " ".r.split(line)
        if (part2) {
          mask = List.fill(5)(mask).mkString("?")
          blocks = List.fill(5)(blocks).mkString(",")
        }
        val blocksList = ",".r.split(blocks).map(_.toInt).toList
        memo = Map()
        val score = process(mask, blocksList)
        ans += score
      }
      println(ans)
    }

  }

  solveDay(12, test = true)
  solveDay(12)

}
