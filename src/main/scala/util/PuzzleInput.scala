package util

import scala.io.Source

object PuzzleInput {
  def readInputFor(day: String): List[String] = {
    val inputSource = Source.fromFile(s"inputs/$day/input.txt")
    val input = inputSource.getLines().toList
    inputSource.close()
    input
  }

  def readTestInputFor(day: String): List[String] = {
    val inputSource = Source.fromFile(s"inputs/$day/test.txt")
    val input = inputSource.getLines().toList
    inputSource.close()
    input
  }
}
