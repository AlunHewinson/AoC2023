package aoc2015

import utils.utils.readDay

import java.security.MessageDigest
import scala.annotation.tailrec
import scala.util.matching.Regex

object day04 extends App {
  // https://adventofcode.com/2015/day/4

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2015)

    def calculateMD5Hash(input: String): String = {
      val md = MessageDigest.getInstance("MD5") // Get MD5 instance
      val bytes = md.digest(input.getBytes) // Compute hash bytes
      val hexString = new StringBuilder // Convert bytes to hexadecimal format
      for (byte <- bytes) {
        val hex = Integer.toHexString(0xff & byte)
        if (hex.length == 1) hexString.append('0')
        hexString.append(hex)
      }
      hexString.toString()
    }

//    println("^0.*".r.pattern.matcher("01234").matches())

    @tailrec
    def findTargetedHash(seed: String, target: Regex, n: Int = 1): Int = {
      val preHash = seed + n
      val hash = calculateMD5Hash(preHash)
      //println(hash)
      if (target.pattern.matcher(hash).matches()) n else findTargetedHash(seed, target, n + 1)
    }

    val answer1 = findTargetedHash(inp, "^00000.*".r)
    val answer2 = findTargetedHash(inp, "^000000.*".r)

    (answer1, answer2)

  }

  println(solveDay(day = 4, test = "test"))
  println(solveDay(day = 4))

}
