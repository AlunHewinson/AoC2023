package aoc2023

import utils.utils.readDay

object day05 extends App {
  // https://adventofcode.com/2023/day/5

  def solveDay(day: Int, test: Boolean = false): (BigInt, BigInt) = {

    val magicMinimum = BigInt("-1")
    val magicMaximum = BigInt("5000000000")

    val inp: String = readDay(day, test, year = 2023)
    val sectionSeparator: String = "(\r*\n){2,}"
    val sections: Array[String] = sectionSeparator.r.split(inp)

    val seeds = " ".r.split(sections.head).tail.map(BigInt(_))
    val seedRangeStarts = seeds.zipWithIndex.flatMap(s => {
      if (s._2 % 2 == 0) Some(s._1) else None
    })
    val seedRangeLengths = seeds.zipWithIndex.flatMap(s => {
      if (s._2 % 2 == 1) Some(s._1) else None
    })
    val seeds2: Page = Page(seedRangeStarts.zip(seedRangeLengths).map(sl => {
      SDMap(Seq(sl._1, sl._1, sl._2))
    }))

    val seedToSoil: Page = Page("\r*\n".r.split(sections(1)).tail.
      map(line => SDMap(" ".r.split(line).map(BigInt(_))))).addMinMax(magicMinimum, magicMaximum)

    val soilToFertiliser: Page = Page("\r*\n".r.split(sections(2)).tail.
      map(line => SDMap(" ".r.split(line).map(BigInt(_))))).addMinMax(magicMinimum, magicMaximum)

    val fertiliserToWater: Page = Page("\r*\n".r.split(sections(3)).tail.
      map(line => SDMap(" ".r.split(line).map(BigInt(_))))).addMinMax(magicMinimum, magicMaximum)

    val waterToLight: Page = Page("\r*\n".r.split(sections(4)).tail.
      map(line => SDMap(" ".r.split(line).map(BigInt(_))))).addMinMax(magicMinimum, magicMaximum)

    val lightToTemperature: Page = Page("\r*\n".r.split(sections(5)).tail.
      map(line => SDMap(" ".r.split(line).map(BigInt(_))))).addMinMax(magicMinimum, magicMaximum)

    val temperatureToHumidity: Page = Page("\r*\n".r.split(sections(6)).tail.
      map(line => SDMap(" ".r.split(line).map(BigInt(_))))).addMinMax(magicMinimum, magicMaximum)

    val humidityToLocation: Page = Page("\r*\n".r.split(sections(7)).tail.
      map(line => SDMap(" ".r.split(line).map(BigInt(_))))).addMinMax(magicMinimum, magicMaximum)


    val locations = seeds.
      map(seedToSoil.mapSource).
      map(soilToFertiliser.mapSource).
      map(fertiliserToWater.mapSource).
      map(waterToLight.mapSource).
      map(lightToTemperature.mapSource).
      map(temperatureToHumidity.mapSource).
      map(humidityToLocation.mapSource)

    val answer1 = locations.min

    val seedToLocation: Page = seedToSoil.
      combine(soilToFertiliser).
      combine(fertiliserToWater).
      combine(waterToLight).
      combine(lightToTemperature).
      combine(temperatureToHumidity).
      combine(humidityToLocation)

    val answer2 = seeds2.combine(seedToLocation).sDMap.map(x => x.targetStart).min

    (answer1, answer2)

  }

  println(solveDay(5, test = true))
  println(solveDay(5))

}

case class SDMap(threeNumbers: Seq[BigInt]) {
  val sourceStart: BigInt = threeNumbers(1)
  val sourceEnd: BigInt = threeNumbers(1) + threeNumbers(2)
  val targetStart: BigInt = threeNumbers.head
  val targetEnd: BigInt = threeNumbers.head + threeNumbers(2)
  val sourceTargetDifference: BigInt = sourceStart - targetStart
  def show(): Unit = {
    println(s"$sourceStart-${sourceEnd-1} -> $targetStart-${targetEnd-1}")
  }
  def mapSource(query: BigInt): Option[BigInt] = {
    if (query >= sourceStart & query < sourceEnd) Some(targetStart + query - sourceStart)
    else None
  }
}

case class Page(sDMap: Seq[SDMap]) {
  def addMinMax(magicMinimum: BigInt, magicMaximum: BigInt): Page = {
    val minStart = this.sDMap.map(sd => sd.sourceStart).min
    val maxEnd = this.sDMap.map(sd => sd.sourceEnd).max
    val minMaxToAdd = Seq(SDMap(Seq(magicMinimum, magicMinimum, minStart - magicMinimum)),
      SDMap(Seq(maxEnd, maxEnd, magicMaximum - maxEnd)))
    this.copy(sDMap = this.sDMap ++ minMaxToAdd)
  }
  def show(): Unit = {
    this.sDMap.foreach(_.show())
  }
  def mapSource(query: BigInt): BigInt = {
    (this.sDMap.flatMap(_.mapSource(query)) :+ query).head
  }
  def combine(next: Page): Page = {
    // first, every combination
    val combinations = for {
      elem1 <- this.sDMap
      elem2 <- next.sDMap
    } yield (elem1, elem2) //println(s"($elem1, $elem2)")

    // only overlapping ones
    val overlaps = combinations.flatMap(c => {
      if (c._1.targetStart <= c._2.sourceEnd-1 & c._1.targetEnd-1 >= c._2.sourceStart) Some(c)
      else None
    })

    // next, find a way to split each overlap
    val newSDMaps: Seq[SDMap] = overlaps.map(o => {
      val newStart = (o._1.targetStart max o._2.sourceStart) + o._1.sourceTargetDifference
      val newEnd = (o._1.targetEnd min o._2.sourceEnd) + o._1.sourceTargetDifference
      val newDiff = o._1.sourceTargetDifference + o._2.sourceTargetDifference
      SDMap(Seq(newStart - newDiff, newStart, newEnd - newStart))
    })

    Page(newSDMaps)

  }
}