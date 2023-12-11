package day5

import scala.io.Source

case class SeedMap(name: String, ranges: List[Range]) {
  def mapValue(value: Int): Option[Int] = {
    ranges.find(r => r.isInRange(value)).map(_.sourceToDestination(value))
  }
}
case class Range(destinationStart: Int, sourceStart: Int, length: Int) {
  private val sourceRange = sourceStart to sourceStart + length
  private val destinationRange = destinationStart to destinationStart + length

  def isInRange(value: Int): Boolean = {
    sourceRange.contains(value)
  }
  def sourceToDestination(value: Int): Int = {
    value match
      case v if sourceRange.contains(v) => destinationRange(sourceRange.indexOf(v))
      case v => v
  }
}

def parseMaps(input: Array[String]): List[SeedMap] = {
  input.map(m => {
    val split = m.split(" map:\n").map(_.trim.stripMargin)
    val ranges = split.tail.flatMap(r => {
      val values = r.split("\n").map(_.split("\\s+").map(_.toInt)).toList
      values.map(v => {
        Range(v.head, v(1), v(2))
      })
    })
    SeedMap(split.head, ranges.toList)
  }).toList
}
@main def day5(): Unit =
  val inputSource = Source.fromFile("inputs/day5/test.txt")
  val input = inputSource.mkString.split("\\n\\n")
  inputSource.close()
  val seeds = "\\d+".r.findAllIn(input.head).map(_.toInt).toList
  println(s"Seeds: ${seeds.mkString(";")}")
  val maps: List[SeedMap] = parseMaps(input.tail)
  println(maps.mkString("\n"))
  println(s"Part 1: ${part1(seeds, maps)}")

  val result = seeds.map(s => {
    for {
      map <- maps
      mapped <- map.mapValue(s)
    } yield mapped
  })

  println(result)


def part1(seeds: List[Int], maps: List[SeedMap]): Int = 0