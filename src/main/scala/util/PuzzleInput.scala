package util

import scala.io.Source

object PuzzleInput {
  def readInputFor(day: String): List[String] = {
    readInputFile(day, "input")
  }

  def readTestInputFor(day: String): List[String] = {
    readInputFile(day, "test")
  }

  def readInputFile(day: String, file: String): List[String] = {
    val inputSource = Source.fromFile(s"inputs/$day/$file.txt")
    val input = inputSource.getLines().toList
    inputSource.close()
    input
  }
}
