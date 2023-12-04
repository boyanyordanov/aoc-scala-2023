import scala.io.Source
object Day3 extends App {
  val inputSource = Source.fromFile("inputs/day3/input.txt")
  val input = inputSource.mkString
  val part1Result = part1(input.linesIterator)
  val part2Result = part2(input.linesIterator)
  inputSource.close()
  println(s"Part 1: $part1Result, Part 2: $part2Result")

  def findPartNumbers(
      lookupRow: String,
      bellowRow: String,
      aboveRow: Option[String] = None
  ): List[Int] = {
    val symbolRegex = "[^A-Za-z0-9.]"
    val numberIndices = "(?=.*)(\\d+)".r.findAllMatchIn(lookupRow).toList

    val foundParts = numberIndices.filter(number => {
      val start = number.start match {
        case 0 => 0
        case v => v - 1
      }

      val end = number.end match {
        case _ if number.end == lookupRow.length => number.end
        case v                                   => v + 1
      }

      s"$symbolRegex(\\d+)|(\\d+)$symbolRegex".r
        .findFirstIn(lookupRow.substring(start, end)) match {
        case Some(_) => true
        case None => {
          val range = bellowRow.substring(start, end)
          symbolRegex.r.findFirstIn(range) match {
            case Some(_) => true
            case None =>
              aboveRow match {
                case Some(row) =>
                  symbolRegex.r.findFirstIn(row.substring(start, end)) match {
                    case Some(_) => true
                    case None    => false
                  }
                case None => false
              }
          }
        }
      }
    })
    foundParts.map(matched => matched.matched.toInt)
  }

  def part1(input: Iterator[String]): Int = {
    val parts = input.toList
    val firstTwo = parts.take(2)
    val lastTwo = parts.takeRight(2)

    val foundParts = findPartNumbers(firstTwo.head, firstTwo.last) ++
      parts
        .sliding(3)
        .flatMap(w => findPartNumbers(w.tail.head, w.last, w.headOption)) ++
      findPartNumbers(lastTwo.last, lastTwo.head, lastTwo.headOption)

    foundParts.sum
  }
  def part2(input: Iterator[String]): Int = {
    val partList = input.toList

    val slide = partList.sliding(3).toList
    val res = slide.flatMap(a => {
      val maybeFirstPart = "\\d+".r.findAllMatchIn(a.head).toList
      val gearRow = "\\d+".r.findAllMatchIn(a.tail.head).toList
      val maybeGears = "\\*".r.findAllMatchIn(a.tail.head).toList
      val maybeSecondPart = "\\d+".r.findAllMatchIn(a.last).toList

      val maybe = maybeGears.map(gear => {
        val first = maybeFirstPart.filter(m => {
          m.start <= gear.end && m.end + 1 >= gear.end
        })

        val second = maybeSecondPart.filter(n =>
          n.start <= gear.end && n.end + 1 >= gear.end
        )

        val gearRowFind = gearRow.filter(l => {
          l.end == gear.start || l.start == gear.end
        })

        var tmp = 0
        first.foreach(f => {
          gearRowFind.foreach(g => {
            tmp = tmp + f.matched.toInt * g.matched.toInt
          })
          second.foreach(s => {
            tmp = tmp + f.matched.toInt * s.matched.toInt
          })
        })

        gearRowFind.foreach(g => {
          second.foreach(s => {
            tmp = tmp + g.matched.toInt * s.matched.toInt
          })
        })

        if (first.length > 1)
          tmp = tmp + first.map(_.matched.toInt).product

        if (second.length > 1)
          tmp = tmp + second.map(_.matched.toInt).product

        if (gearRowFind.length > 1)
          tmp = tmp + gearRowFind.map(_.matched.toInt).product

        tmp
      })

      maybe
    })

    res.sum
  }
}
