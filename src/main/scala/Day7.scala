package day7

import util.PuzzleInput.{readInputFor, readTestInputFor, readInputFile}

@main def day7(): Unit =
    val input = readInputFor("day7")
    println(s"Part 1: ${part1(input)}, Part 2: ${part2(input)}")

case class Hand(content: List[String], bid: Int) {
  val score: Int = {
    val cardMap = content.groupBy(identity).view.mapValues(_.size).toMap
    evalScore(cardMap)
  }

  private def evalScore(cardMap: Map[String, Int]) = {
    cardMap.values.toList.sorted.reverse match
      case 5 :: Nil => 6
      case 4 :: 1 :: Nil => 5
      case 3 :: 2 :: Nil => 4
      case 3 :: 1 :: 1 :: Nil => 3
      case 2 :: 2 :: 1 :: Nil => 2
      case 2 :: 1 :: 1 :: 1 :: Nil => 1
      case _ => 0
  }

  val scoreWithJokers: Int = {
    val cardMap = content.groupBy(identity).view.mapValues(_.size).toMap
    val jokers = cardMap.getOrElse("J", 0)
    val noJokersMap = cardMap.filter((k, _) => k != "J")

    if(noJokersMap.isEmpty) {
      6
    } else {
      val maxCard = noJokersMap.maxBy(_._2)

      val updatedMap = noJokersMap.updated(maxCard._1, maxCard._2 + jokers)
      val newScore = evalScore(updatedMap)

//      println(content)
//      println(cardMap.mkString(", "))
//      println(updatedMap.mkString(", "))
//      println(s"Score: $score")
//      println(s"New Score: $newScore")
      newScore
    }
  }

  def compareContent(otherHand: Hand, cards: String = "23456789TJQKA"): Int = {
    content.zip(otherHand.content).find((card1, card2) => {
      val cardScore = cards.indexOf(card1)
      val otherScore = cards.indexOf(card2)
      cardScore != otherScore
    }).map { case (card1, card2) =>
      val cardScore = cards.indexOf(card1)
      val otherScore = cards.indexOf(card2)
      cardScore.compare(otherScore)
    }.getOrElse(0)
  }

  def compareHand(otherHand: Hand): Int = {
    if(score == otherHand.score) {
      compareContent(otherHand)
    } else score.compare(otherHand.score)
  }

  def compareHandWithJokers(otherHand: Hand): Int = {
    if (scoreWithJokers == otherHand.scoreWithJokers) {
      compareContent(otherHand, "J23456789TQKA")
    } else scoreWithJokers.compare(otherHand.scoreWithJokers)
  }
}

object Hand {
  implicit def ordering: Ordering[Hand] = (x: Hand, y: Hand) => x.compareHand(y)
}

def parseInput(value: List[String]): List[Hand] = {
  value.map(h => {
    val s = h.split("\\s+")
    Hand(s.head.split("").toList, s.last.toInt)
  })
}

def calculateWinnings(hand: Hand, score: Int) = hand.bid * (score + 1)

def part1(input: List[String]): Int = {
  parseInput(input).sorted
    .zipWithIndex
    .map(calculateWinnings)
    .sum
}

def part2(input: List[String]): Int = {
  parseInput(input).sorted((x: Hand, y: Hand) => x.compareHandWithJokers(y))
    .zipWithIndex
    .map(calculateWinnings)
    .sum
}