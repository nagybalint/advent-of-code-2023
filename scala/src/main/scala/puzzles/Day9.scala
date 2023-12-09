package puzzles

import scala.annotation.tailrec

object Day9 {
  private def parseTimeSeries(dataLine: String): Seq[Int] = dataLine.split(" ").map(_.strip().toInt)

  def extrapolate(timeSeries: Seq[Int]): Int = {
    @tailrec
    def go(timeSeries: Seq[Int], base: Int): Int = {
      val diffs = timeSeries.sliding(2).map(x => x.last - x.head).toSeq
      if (diffs.forall(_ == 0)) base else go(diffs, base + diffs.last)
    }
    go(timeSeries, timeSeries.last)
  }

  def task1(dataLines: Seq[String]): Int = dataLines.map(parseTimeSeries).map(extrapolate).sum
  def task2(dataLines: Seq[String]): Int = dataLines.map(parseTimeSeries).map(_.reverse).map(extrapolate).sum
}
