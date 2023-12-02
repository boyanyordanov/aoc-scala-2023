import scala.io.Source

object Day2 extends App {
  val inputSource = Source.fromFile("inputs/day2/input.txt")
  val input = inputSource.mkString
  val part1 = part1Solution(input.linesIterator)
  val part2 = part2Solution(input.linesIterator)
  inputSource.close()

  println(s"Part 1: $part1, Part 2: $part2")

  case class DrawnCubes(color: String, count: Int)
  case class Game(id: Int, drawings: List[DrawnCubes])

  private def parseLine(line: String) = {
    val gameData = line.split(":")
    val id = gameData.head.split(" ").last.toInt
    val drawnCubes = gameData.last
      .split(";")
      .map(_.trim.stripMargin)
      .map(_.trim.split(", "))
      .flatMap(
        _.map(_.split(" ")).map(cubes =>
          DrawnCubes(cubes.last, cubes.head.toInt)
        )
      )
      .toList

    Game(id, drawnCubes)
  }

  private def isGamePossible(game: Game): Boolean = {
    val maxCubes = Map("red" -> 12, "green" -> 13, "blue" -> 14)
    game.drawings.exists(draw => draw.count > maxCubes.getOrElse(draw.color, 0))
  }
  private def part1Solution(input: Iterator[String]): Int =
    input.map(parseLine).filter(isGamePossible).map(_.id).sum

  private def part2Solution(input: Iterator[String]): Int =
    input
      .map(parseLine)
      .map(game => {
        val maxBlue = game.drawings.filter(_.color == "blue").map(_.count).max
        val maxRed = game.drawings.filter(_.color == "red").map(_.count).max
        val maxGreen = game.drawings.filter(_.color == "green").map(_.count).max

        maxBlue * maxRed * maxGreen
      })
      .sum
}
