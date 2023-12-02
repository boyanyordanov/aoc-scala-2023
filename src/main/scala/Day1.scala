import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex
import cats.implicits._

object Day1 extends App {
  private def part1(input: Iterator[String]): Int = input.map(_.toList.filter(_.isDigit))
    .map(a => a.take(1).concat(a.takeRight(1)).mkString.toIntOption.fold(0)(i => i))
    .sum

  private def part2(input: Iterator[String]): Int = {
    val digits: Map[String, String] = Map(
      "one" -> "1",
      "two" -> "2",
      "three" -> "3",
      "four" -> "4",
      "five" -> "5",
      "six" -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine" -> "9",
    )

    @tailrec
    def replaceDigits(str: String, fromIndex: Int = 0, result: String = ""): String = {
      val regex = s"[1-9]|${digits.keys.mkString("|")}"
      regex.r.findFirstMatchIn(str.substring(fromIndex)) match {
        case Some(matched) => replaceDigits(
          str,
          fromIndex + matched.start + 1,
          s"$result${digits.getOrElse(matched.matched, matched.matched)}"
        )
        case None => result
      }
    }

    part1(input.map(a => replaceDigits(a)))
  }

  private def part2Alt(input: Iterator[String]): Int = {
    val digits: Map[String, String] = Map(
      "one" -> "1",
      "two" -> "2",
      "three" -> "3",
      "four" -> "4",
      "five" -> "5",
      "six" -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine" -> "9",
    )

    val regex = s"${(1 to 9).mkString("|")}|${digits.keys.mkString("|")}"

    def findDigits(str: String): String = {
      val left = regex.r.findFirstIn(str).map(c => digits.getOrElse(c, c)).getOrElse("")
      val right = regex.reverse.r.findFirstIn(str.reverse).map(c => digits.getOrElse(c.reverse, c)).getOrElse("")
      s"$left$right"
    }

    input
      .map(findDigits)
      .map(_.toIntOption)
      .fold(0.some)((a, c) => a |+| c)
      .getOrElse(0)
  }

  val input = Source.fromFile("inputs/day1/input.txt").mkString
  println(part1(input.linesIterator))
  println(part2(input.linesIterator))
  println(part2Alt(input.linesIterator))
}
