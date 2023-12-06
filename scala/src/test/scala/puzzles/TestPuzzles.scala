package puzzles

import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source
import scala.util.{Failure, Success, Using}

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

  "Day 4, Task 1" in {
    val dataLines = readInput("/input_day4.txt")
    val response = Day4.task1(dataLines)
    assert(response == 24848)
  }

  "Day 4, Task 2" in {
    val dataLines = readInput("/input_day4.txt")
    val response = Day4.task2(dataLines)
    assert(response == 7258152)
  }

  "Day 5, Task 1" in {
    val dataLines = readInput("/input_day5.txt")
    val response = Day5.task1(dataLines)
    assert(response == 836040384)
  }

  "Day 5, Task 2" in {
    val dataLines = readInput("/input_day5.txt")
    val response = Day5.task2(dataLines)
    assert(response == 10834440)
  }

  "Day 6, Task 1" in {
    val dataLines = readInput("/input_day6.txt")
    val response = Day6.task1(dataLines)
    assert(response == 1155175)
  }

  "Day 6, Task 2" in {
    val dataLines = readInput("/input_day6.txt")
    val response = Day6.task2(dataLines)
    assert(response == 35961505)
  }

  private def readInput(fileName: String): Seq[String] = {
    Using(Source.fromURL(getClass.getResource(fileName))) { source =>
      source.getLines.foldLeft(Seq.empty[String])((acc, x) => acc ++ Seq(x))
    } match {
      case Failure(exception) => throw exception
      case Success(dataLines) => dataLines
    }
  }
}