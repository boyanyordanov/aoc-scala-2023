import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {
  val inputSource = Source.fromFile("inputs/day4/input.txt")
  val input = inputSource.getLines().toList
  inputSource.close()
  val part1 = part1Solution(input)
  val part2 = part2Solution(input)

  println(s"Part 1: $part1, Part 2: $part2")


  case class Card(id: Int, winningNumbers: List[Int], playerNumbers: List[Int]) {
    def findGuessedNumbers: List[Int] = {
      playerNumbers.intersect(winningNumbers)
    }

    def score: Int = {
      math.pow(2.toDouble, (findGuessedNumbers.length - 1).toDouble).toInt
    }
  }

  def parseCards(possibleCards: List[String]): List[Card] = {
    possibleCards.map(line => {
      val split = line.split(":")
      val id = "Card\\s+(\\d+)".r.findFirstMatchIn(split.head).map(_.group(1).toInt).get
      val numbers = split.tail.last.split("\\|")
      val winning = "\\d+".r.findAllMatchIn(numbers.head).map(_.matched.toInt).toList
      val player = "\\d+".r.findAllMatchIn(numbers.last).map(_.matched.toInt).toList

      Card(id, winning, player)
    })
  }

  def part1Solution(possibleCards: List[String]) = {
    println("Working on solution to part 1...")
    val cards: List[Card] = parseCards(possibleCards)
    cards.map(_.score).sum
  }

  def part2Solution(possibleCards: List[String]) = {
    println("Working on solution to part 2...")
    val drawnCards: List[Card] = parseCards(possibleCards)

    @tailrec
    def cardsWon(cards: List[Card], wonCards: List[Card] = List.empty): List[Card] = {
      if(cards.isEmpty)
        wonCards
      else {
        val c = cards.flatMap(card => {
          val cardNumber = card.id + card.findGuessedNumbers.length
          val newCards: List[Card] = drawnCards.collect {
            case c if c.id > card.id && c.id <= cardNumber => c
          }
          newCards
        })
        cardsWon(c, wonCards ++ c)
      }
    }

    drawnCards.length + cardsWon(drawnCards).length
  }
}
