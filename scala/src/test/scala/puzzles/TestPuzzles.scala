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

  "Day 7, Task 1" in {
    val dataLines = readInput("/input_day7.txt")
    val response = Day7.task1(dataLines)
    assert(response == 251136060)
  }

  "Day 7, Task 2" in {
    val dataLines = readInput("/input_day7.txt")
    val response = Day7.task2(dataLines)
    assert(response == 249400220)
  }

  "Day 8, Task 1" in {
    val dataLines = readInput("/input_day8.txt")
    val response = Day8.task1(dataLines)
    assert(response == 20221)
  }

  "Day 8, Task 2" in {
    val dataLines = readInput("/input_day8.txt")
    val response = Day8.task2(dataLines)
    assert(response == 14616363770447L)
  }

  "Day 9, Task 1" in {
    val dataLines = readInput("/input_day9.txt")
    val response = Day9.task1(dataLines)
    assert(response == 1681758908)
  }

  "Day 9, Task 2" in {
    val dataLines = readInput("/input_day9.txt")
    val response = Day9.task2(dataLines)
    assert(response == 803)
  }

  "Day 10, Task 1" in {
    val dataLines = readInput("/input_day10.txt")
    val response = Day10.task1(dataLines)
    assert(response == 7012)
  }

  "Day 10, Task 2" in {
    val dataLines = readInput("/input_day10.txt")
    val response = Day10.task2(dataLines)
    assert(response == 395)
  }

  "Day 11, Task 1" in {
    val dataLines = readInput("/input_day11.txt")
    val response = Day11.task1(dataLines)
    assert(response == 10276166)
  }

  "Day 11, Task 2" in {
    val dataLines = readInput("/input_day11.txt")
    val response = Day11.task2(dataLines)
    assert(response == 598693078798L)
  }

  "Day 12, Task 1" in {
    val dataLines = readInput("/input_day12.txt")
    val response = Day12.task1(dataLines)
    assert(response == 8270)
  }

  "Day 12, Task 2" in {
    val dataLines = readInput("/input_day12.txt")
    val response = Day12.task2(dataLines)
    assert(response == 204640299929836L)
  }

  "Day 13, Task 1" in {
    val dataLines = readInput("/input_day13.txt")
    val response = Day13.task1(dataLines)
    assert(response == 29165)
  }

  "Day 13, Task 2" in {
    val dataLines = readInput("/input_day13.txt")
    val response = Day13.task2(dataLines)
    assert(response == 32192)
  }

  "Day 14, Task 1" in {
    val dataLines = readInput("/input_day14.txt")
    val response = Day14.task1(dataLines)
    assert(response == 109596)
  }

  "Day 14, Task 2" in {
    val dataLines = readInput("/input_day14.txt")
    val response = Day14.task2(dataLines)
    assert(response == 96105)
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