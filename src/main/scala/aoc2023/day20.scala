package aoc2023

import utils.utils.readDay

import scala.annotation.tailrec

object day20 extends App {
  // https://adventofcode.com/2023/day/20

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val lineSeparator: String = "\r*\n"
    val lines: Seq[String] = lineSeparator.r.split(inp)

    val modules: Seq[Module] = lines.map {
      case s"%$name -> $targetList" => Module(name, "%", targetList) // flip flop
      case s"&$name -> $targetList" => Module(name, "&", targetList) // conjunction
      case s"broadcaster -> $targetList" => Module("broadcaster", "B", targetList) // broadcaster
    }
    val inputMap: Map[String, Seq[(String, String)]] = modules.flatMap(m => {
      val from = m.name
      m.targets.map(t => (from, t))
    }).groupBy(_._2)

    val modulesWithInputs: Seq[Module] = modules.map(m => {
      if (m.kind == "&") {
        val inputs: Seq[(String, Int)] = inputMap(m.name).map(i => (i._1, 0))
        m.copy(sourceMemory = inputs)
      } else m
    })

    val rxFeeders: Seq[RXFeeder] = modulesWithInputs
      .filter(_.targets.contains("rx"))
      .map(x => RXFeeder(x.name, Seq(BigInt(1))))
    val rxFeederFeeders: Seq[RXFeeder] = modulesWithInputs
      .filter(_.targets.intersect(rxFeeders.map(_.name)).nonEmpty)
      .map(x => RXFeeder(x.name, Seq(BigInt(1))))

    @tailrec
    def process(queue: Seq[QueueItem], modules: Seq[Module], n: Int = 1, acc: (Int, Int, Int) = (0, 0, 0)): (Int, Int, Int) = {
      //println("in process:")
      //println(acc._3)
      //println(acc)
      //queue.foreach(_.show())
      if (n == 0) {
        //println("n is zero, quitting")
        acc
      }
      else if (queue.isEmpty) {
        //println("queue is empty, iterating n down by 1")
        process(Seq(QueueItem("button", 0, "broadcaster")), modules, n-1, (acc._1, acc._2, acc._3 + 1))
      }
      else {
        val firstQueueItem = queue.head
        //println(firstQueueItem.toModule)
        val targetModule: Option[Module] = modules.find(_.name == firstQueueItem.toModule)
        if (targetModule.isEmpty) {
          //println(s"no target module found ->${firstQueueItem.toModule}<- (must be a node without outputs)")
          process(queue.tail, modules, n, (acc._1 + 1 - firstQueueItem.signal, acc._2 + firstQueueItem.signal, acc._3))
        }
        else {
          //println("processing the head of the queue")
          val otherModules = modules.filter(_.name != firstQueueItem.toModule)
          if (targetModule.get.name == "rx" & firstQueueItem.signal == 0) {
            println(s" -->  $n  <-- ")
          }
          val moduleQueue = targetModule.get.processInput(firstQueueItem.signal, firstQueueItem.sourceModule)
          if (targetModule.get.name == "fzvjddcdg" & firstQueueItem.signal == 0) {
            if (test == "") {
              print(s"${acc._3} ")
              targetModule.get.show()
            }
          }
          val newModules = otherModules :+ moduleQueue._1
          val newHighs = firstQueueItem.signal
          val newLows = 1 - firstQueueItem.signal
          process(queue.tail ++ moduleQueue._2, newModules, n, (acc._1 + newLows, acc._2 + newHighs, acc._3))
        }
      }

    }

    @tailrec
    def process2(queue: Seq[QueueItem], modules: Seq[Module], n: Int = 1, acc: Seq[RXFeeder]): Seq[RXFeeder] = {
      val sanityLimit = 20000
      if (n >= sanityLimit) {
        println(s"FAILED TO FIND AN ANSWER IN THE FIRST $sanityLimit ITERATIONS!")
        acc
      } else if (acc.map(_.lows.length).min >= 3) {
        acc
      } else if (queue.isEmpty) {
        process2(Seq(QueueItem("button", 0, "broadcaster")), modules, n + 1, acc)
      } else {
        val firstQueueItem = queue.head
        val targetModule: Option[Module] = modules.find(_.name == firstQueueItem.toModule)
        if (targetModule.isEmpty) {
          process2(queue.tail, modules, n, acc)
        }
        else {
          val otherModules = modules.filter(_.name != firstQueueItem.toModule)
          val moduleQueue = targetModule.get.processInput(firstQueueItem.signal, firstQueueItem.sourceModule)
          val newAcc = if (acc.map(_.name).contains(targetModule.get.name) & firstQueueItem.signal == 0) {
            acc.filter(_.name != targetModule.get.name) ++
              acc.filter(_.name == targetModule.get.name).map(x => x.copy(lows = x.lows :+ BigInt(n)))
          } else acc

          val newModules = otherModules :+ moduleQueue._1
          process2(queue.tail ++ moduleQueue._2, newModules, n, newAcc)
        }
      }
    }


    val a1help: (Int, Int, Int) = process(Seq(QueueItem("button", 0, "broadcaster")), modulesWithInputs, 1000)
    val answer1 = BigInt(a1help._1) * a1help._2

    val answer2 = if (test == "") {
      val a2help: Seq[RXFeeder] = process2(Seq(QueueItem("button", 0, "broadcaster")), modulesWithInputs, 1, rxFeederFeeders)
      a2help.map(a => a.lows(2) - a.lows(1)).product
    } else BigInt(0)

    (answer1, answer2)

  }

  println(solveDay(day = 20, test = "test2"))
  println(solveDay(day = 20))

}

case class RXFeeder(name: String, lows: Seq[BigInt])
case class QueueItem(sourceModule: String, signal: Int, toModule: String) {
  def show(): Unit = {
    println(s"queue item: $sourceModule --> $signal --> $toModule")
  }
}
case class Module(name: String, kind: String, targets: Seq[String], state: Int = 0, sourceMemory: Seq[(String, Int)] = Seq()) {
  def show(): Unit = {
    kind match {
      case "B" => println(s"$name  -->  ${targets.mkString(", ")}")
      case "%" => println(s"$kind $name ($state)  -->  ${targets.mkString(", ")}")
      case "&" => println(s"$kind $name [$sourceMemory]  -->  ${targets.mkString(", ")}")
    }
  }
  def processInput(signal: Int, sourceModule: String): (Module, Seq[QueueItem]) = {
    (this.kind, signal) match {
      case ("%", 0) => (this.copy(state = 1 - this.state), this.targets.map(x => QueueItem(this.name, 1 - this.state, x)))
      case ("%", 1) => (this, Seq())
      case ("&", n) =>
        val otherSources: Seq[(String, Int)] = this.sourceMemory.filterNot(_._1 == sourceModule)
        val thisSource: Seq[(String, Int)] = this.sourceMemory.filter(_._1 == sourceModule).map(x => (x._1, n))
        val combinedSources: Seq[(String, Int)] = otherSources ++ thisSource
        val outputSignal: Int = 1 - combinedSources.map(_._2).product
        (this.copy(sourceMemory = combinedSources), this.targets.map(x => QueueItem(this.name, outputSignal, x)))
      case ("B", n) => (this, this.targets.map(x => QueueItem(this.name, n, x)))
    }
  }
}
object Module {
  def apply(name: String, kind: String, targetList: String): Module = {
    val targets: Seq[String] = ", ".r.split(targetList)
    new Module(name, kind, targets)
  }
}