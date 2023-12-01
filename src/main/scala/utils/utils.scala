package utils

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

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

  case class Lens[A, B](get: A => B, set: (A, B) => A)

  def compose[Outer, Inner, Value](
                                    outer: Lens[Outer, Inner],
                                    inner: Lens[Inner, Value]
                                  ): Lens[Outer, Value] = Lens[Outer, Value](
    get = outer.get andThen inner.get,
    set = (obj, value) => outer.set(obj, inner.set(outer.get(obj), value))
  )

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b==0) a.abs else gcd(b, a%b)
  }

  def lcm(a: Int, b: Int): Int = {
    (a*b).abs/gcd(a,b)
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
//      println("vvvvvvv")
//      println(this)
//      println(other)
//      println(times)
//      println(this add other)
//      println("^^^^^^^")
      if (times == 0) this
      else if (times == 1) this add other
      else this.repeatedlyAdd(other, times - 1) add other
    }
  }


}
