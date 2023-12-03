package puzzles

import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class TestPuzzles extends AnyWordSpec {
  "Day 1, Task 1" in {
    val dataLines = readInput("/input_day1.txt")
    val response = Day1.task1(dataLines)
    assert(response == 54667)
  }

  "Day 1, Task 2" in {
    val dataLines = readInput("/input_day1.txt")
    val response = Day1.task2(dataLines)
    assert(response == 54203)
  }

  "Day 2, Task 1" in {
    val dataLines = readInput("/input_day2.txt")
    val response = Day2.task1(dataLines)
    assert(response == 2285)
  }

  "Day 2, Task 2" in {
    val dataLines = readInput("/input_day2.txt")
    val response = Day2.task2(dataLines)
    assert(response == 77021)
  }

  "Day 3, Task 1" in {
    val dataLines = readInput("/input_day3.txt")
    val response = Day3.task1(dataLines)
    assert(response == 544433)
  }

  "Day 3, Task 2" in {
    val dataLines = readInput("/input_day3.txt")
    val response = Day3.task2(dataLines)
    assert(response == 76314915)
  }

  private def readInput(fileName: String) = {
    val source = Source.fromURL(getClass.getResource(fileName))
    val dataLines = source.getLines.foldLeft(Seq.empty[String])((acc, x) => acc ++ Seq(x))
    source.close()
    dataLines
  }
}