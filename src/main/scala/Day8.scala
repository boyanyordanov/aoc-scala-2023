package Day8

import util.PuzzleInput.{readInputFile, readInputFor, readTestInputFor}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

@main def day8(): Unit =
  val input = readInputFor("day8")
  val part2Test = readInputFile("day8", "test_part2")
  val s = System.currentTimeMillis()
  println(s"Part 1: ${part1(input)}, Part 2: ${part2(input)}")
  println(System.currentTimeMillis() - s)

case class Node(left: String, right: String)

def parseCoords(input: List[String]) = {
  val directions = input.head.split("")
  val coords = input.tail.tail

  val parsed = ListMap.from(coords.map(row => {
    val split = row.split(" = ")
    val coord = split.head
    val parts = "\\(([1-9A-Z]+), ([1-9A-Z]+)\\)".r.findAllIn(split.tail.mkString)
    (coord -> Node(parts.group(1), parts.group(2)))
  }))
  (directions, parsed)
}
def part1(input: List[String]): Long = {
  val (directions: Array[String], parsed: ListMap[String, Node]) = parseCoords(input)
  val (_, initial) = parsed.find((k, _) => k == "AAA").get
  implicit val map: ListMap[String, Node] = parsed
  implicit def predicate: String => Boolean = str => str == "ZZZ"
  countSteps(initial, directions)
}


@tailrec
def countSteps(current: Node, directions: Array[String], count: Long = 1)(implicit map: ListMap[String, Node], endCondition: String => Boolean): Long = {
  val next = directions.head match
    case "L" => current.left
    case "R" => current.right

  val upcoming = map(next)
  if (endCondition(next)) return count
  countSteps(upcoming, directions.tail.appended(directions.head), count + 1)
}

def part2(input: List[String]): Long = {
  val (directions: Array[String], parsed: ListMap[String, Node]) = parseCoords(input)
  val startPoints = parsed.filter((k, _) => k.endsWith("A"))

  implicit val map: ListMap[String, Node] = parsed
  implicit def predicate: String => Boolean = str => str.endsWith("Z")
  val results = startPoints.map(s => {
    countSteps(s._2, directions)
  }).toList.sorted

  //      1 798 142 903
  // 18 215 611 419 223
  // 18 215 611 419 223

  @tailrec
  def gcd(a: Long, b: Long): Long = {
    // If the second argument is 0, return the first argument (base case)
    if (b == 0) {
      return a;
    }
    // Otherwise, recursively call gcd with arguments b and the remainder of a divided by b
    gcd(b, a % b);
  }

  results.reduce((a, b) => a * b / gcd(a, b))
}

