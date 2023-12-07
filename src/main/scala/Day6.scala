import util.PuzzleInput.readInputFor

@main def day6(): Unit =
  val input = readInputFor("day6")
  println(s"Part 1: ${part1(input)}, Part 2: ${part2(input)}")


case class Race(length: Long, recordDistance: Long) {
  def possibleWins: Long = {
    (1L to length).count(i => (i * (length - i)) > recordDistance)
  }
}
def parseRaces(lines: List[String]): List[Race] = {
  val lengths = "(\\d+)".r.findAllIn(lines.head).map(_.toLong).toList
  val records = "(\\d+)".r.findAllIn(lines(1)).map(_.toLong).toList

  lengths.zipWithIndex.map((el, i) => Race(el, records(i)))
}
def part1(input: List[String]): Long = {
  val races = parseRaces(input)
  races.map(_.possibleWins).product
}

def part2(input: List[String]): Long = {
  val raceLength = "\\d+.+".r.findFirstIn(input.head).map(_.replaceAll("\\s", "").toLong)
  val raceDistance = "\\d+.+".r.findFirstIn(input(1)).map(_.replaceAll("\\s", "").toLong)
  Race(raceLength.getOrElse(0L), raceDistance.getOrElse(0L)).possibleWins
}