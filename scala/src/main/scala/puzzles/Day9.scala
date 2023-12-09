package puzzles

import scala.annotation.tailrec

object Day9 {
  def extrapolate(ts: Seq[Int]): Int = {
    @tailrec
    def go(ts: Seq[Int], agg: Int): Int = {
      val diffs = ts.sliding(2).map { case Seq(a, b) => b - a }.toSeq
      if (diffs.forall(_ == 0)) agg else go(diffs, agg + diffs.last)
    }
    go(ts, ts.last)
  }
  def parseTimeSeries(in: String): Seq[Int] = in.split(" ").map(_.strip().toInt)
  def task1(in: Seq[String]): Int = in.map(parseTimeSeries).map(extrapolate).sum
  def task2(in: Seq[String]): Int = in.map(parseTimeSeries).map(_.reverse).map(extrapolate).sum
}
