package puzzles

import scala.util.Try

object Day1 {
  val DIGITS = Seq("zero" ,"one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  def task1(input: Seq[String]): Int =
    input.map(_.toCharArray.filter(_.isDigit)).map(x => x.take(1).appended(x.last).mkString.toInt).sum

  def getFirstWrittenDigit(in: String): Option[(Int, Int)] =
    Try(DIGITS.map(digit => in.indexOf(digit)).zipWithIndex.filterNot(_._1 == -1).minBy(_._1)).toOption

  def getLastWrittenDigit(in: String): Option[(Int, Int)] =
    Try(DIGITS.map(digit => in.lastIndexOf(digit)).zipWithIndex.filterNot(_._1 == -1).maxBy(_._1)).toOption

  def getFirstRealDigit(in: String): (Int, Int) = {
    val (position, digit) = in.toCharArray.zipWithIndex.filter(_._1.isDigit).head.swap
    (position, digit.toString.toInt)
  }

  def getLastRealDigit(in: String): (Int, Int) = {
    val (position, digit) = in.toCharArray.zipWithIndex.filter(_._1.isDigit).last.swap
    (position, digit.toString.toInt)
  }

  def task2(input: Seq[String]): Int = {
    input.map(x => {
      val fwd = getFirstWrittenDigit(x)
      val frd = getFirstRealDigit(x)
      val lwd = getLastWrittenDigit(x)
      val lrd = getLastRealDigit(x)
      val fd = fwd match {
        case Some(wd) => if (wd._1 < frd._1) wd._2 else frd._2
        case None => frd._2
      }
      val ld = lwd match {
        case Some(wd) => if (wd._1 > lrd._1) wd._2 else lrd._2
        case None => lrd._2
      }
      fd * 10 + ld
    }).sum
  }
}
