package utils

import java.io.FileNotFoundException
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.math.abs

object utils {
  def readDay(day: Int, test: Boolean = false, year: Int): String = {
    val inputFileOldStyle: String = s"input/$year/day$day${if (test) "test" else ""}.txt"
    val inputFileNewStyle: String = s"input/$year/${if (test) "test" else "day"}$day.txt"
    val inputFileNewerStyle: String = s"input/$year/adventofcode.com_${year}_day_${day}_${if (test) "test" else "input"}.txt"

    try {
      Using(Source.fromFile(inputFileOldStyle)) { source => source.mkString }.get
    } catch {
      case noOld: Throwable => try {
        Using(Source.fromFile(inputFileNewStyle)) { source => source.mkString }.get
      } catch {
        case noNew: Throwable => try {
          Using(Source.fromFile(inputFileNewerStyle)) { source => source.mkString }.get
        } catch {
          case noNewer: Throwable => {
            println("input file not found")
            ""
          }
        }
      }
    }
  }

  def readDay(day: Int, test: String, year: Int): String = {
    val inputFileOldStyle: String = s"input/$year/day$day$test.txt"

    try {
      Using(Source.fromFile(inputFileOldStyle)) { source => source.mkString }.get
    } catch {
      case noNewer: Throwable => {
        throw new FileNotFoundException(s"input/$year/day$day$test.txt")
      }
    }
  }

  case class Lens[A, B](get: A => B, set: (A, B) => A)

  def compose[Outer, Inner, Value](
                                    outer: Lens[Outer, Inner],
                                    inner: Lens[Inner, Value]
                                  ): Lens[Outer, Value] = Lens[Outer, Value](
    get = outer.get andThen inner.get,
    set = (obj, value) => outer.set(obj, inner.set(outer.get(obj), value))
  )

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = {
    if (b==0) a.abs else gcd(b, a % b)
  }

  def gcd(numbers: Array[BigInt]): BigInt = {
    def gcdInner(a: BigInt, b: BigInt): BigInt = {
      if (b == 0) a.abs else gcdInner(b, a % b)
    }
    numbers.reduceLeft((acc, num) => gcdInner(acc, num))
  }

  def lcm(a: BigInt, b: BigInt): BigInt = {
    (a * b).abs / gcd(a, b)
  }

  def lcm(numbers: Array[BigInt]): BigInt = {
    def lcmInner(a: BigInt, b: BigInt): BigInt = {
      (a * b).abs / gcd(a, b)
    }
    numbers.reduceLeft((acc, num) => lcmInner(acc, num))
  }

  import java.io.File
  import java.nio.file.{Paths, Files}

  def createEmptyTextFiles(year: Int) = {
    (1 to 25).map(day => {
      val filePath = s"input/$year/$day.txt"
      val file = new File(filePath)

      try {
        if (!file.exists()) {
          Files.createFile(Paths.get(filePath))
          println(s"Created $year day $day successfully")
        } else {
          println(s"$year day $day already exists.")
        }
      } catch {
        case e: Exception => println("An error occurred: " + e.getMessage)
      }
    })
  }

  case class Coord(row: Int, column: Int) {
    def nw(n: Int = 1): Coord = Coord(this.row - n, this.column - n)
    def n(n: Int = 1):  Coord = Coord(this.row - n, this.column + 0)
    def ne(n: Int = 1): Coord = Coord(this.row - n, this.column + n)
    def w(n: Int = 1):  Coord = Coord(this.row + 0, this.column - n)
    def e(n: Int = 1):  Coord = Coord(this.row + 0, this.column + n)
    def sw(n: Int = 1): Coord = Coord(this.row + n, this.column - n)
    def s(n: Int = 1):  Coord = Coord(this.row + n, this.column + 0)
    def se(n: Int = 1): Coord = Coord(this.row + n, this.column + n)
    def add(other: Coord): Coord = Coord(this.row + other.row, this.column + other.column)
    def subtract(other: Coord): Coord = Coord(this.row - other.row, this.column - other.column)
    def p(): Unit = println(this.row.toString + "," + this.column.toString)
    def mk(): String = this.row.toString + "," + this.column.toString
    def distance(other: Coord): Coord = Coord(math.abs(this.row - other.row), math.abs(this.column - other.column))
    def manhattenMagnitude(): Int = math.abs(this.row) + math.abs(this.column)
    def rotate(angle: Int): Coord = {
      angle match {
        case 0 => this
        case 90 => Coord(this.column, -this.row)
        case 180 => Coord(-this.row, -this.column)
        case 270 => Coord(-this.column, this.row)
      }
    }
    final def repeatedlyAdd(other: Coord, times: Int = 1): Coord = {
      if (times == 0) this
      else if (times == 1) this add other
      else this.repeatedlyAdd(other, times - 1) add other
    }
  }

}

case class BigAddress(r: BigInt, c: BigInt) {
  def east(n: BigInt = 1): BigAddress = BigAddress(this.r, this.c + n)
  def south(n: BigInt = 1): BigAddress = BigAddress(this.r + n, this.c)
  def west(n: BigInt = 1): BigAddress = BigAddress(this.r, this.c - n)
  def north(n: BigInt = 1): BigAddress = BigAddress(this.r - n, this.c)
}
case class Address(r: Int, c: Int) {
  def show(): Unit = {
    println((r, c))
  }
  def east(n: Int = 1): Address  = Address(this.r, this.c + n)
  def south(n: Int = 1): Address = Address(this.r + n, this.c)
  def west(n: Int = 1): Address  = Address(this.r, this.c - n)
  def north(n: Int = 1): Address = Address(this.r - n, this.c)

  def expandedManhattenDistance(that: Address, expandedRowWeights: Seq[(Int, BigInt)], expandedColumnWeights: Seq[(Int, BigInt)]): BigInt = {
    val rs = Seq(this.r, that.r).sorted
    val cs = Seq(this.c, that.c).sorted

    val relevantExpandingRows = expandedRowWeights.filter(rw => rw._1 > rs.head & rw._1 < rs.last)
    val relevantExpandingColumns = expandedColumnWeights.filter(cw => cw._1 > cs.head & cw._1 < cs.last)

    val distance = rs.last - rs.head + relevantExpandingRows.map(er => er._2).sum +
      cs.last - cs.head + relevantExpandingColumns.map(ec => ec._2).sum

    distance
  }
}