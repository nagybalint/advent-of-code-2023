package puzzles

import scala.annotation.tailrec

object Day13 {
  type Pattern = List[String]
  type SmudgePosition = (Int, Int)

  def findHorizontalReflectionPoint(pattern: Pattern): Seq[Int] =
    (1 until pattern.size).filter(idx =>
      (0 to Math.min(idx - 1, pattern.length - idx - 1))
        .forall(i => pattern(idx - i - 1) == pattern(idx + i)))

  def parsePatterns(in: Seq[String]): Seq[Pattern] =
    in.foldRight(Seq(List.empty[String])) { case (l, head :: tail ) => l match {
      case l if l.isBlank => List.empty :: head :: tail
      case l => head.prepended(l) :: tail
    }}

  def patternSummaries(pattern: Pattern): Seq[Int] = {
    val horizontalReflectionPoints = findHorizontalReflectionPoint(pattern).map(_ * 100)
    val verticalReflectionPoints = findHorizontalReflectionPoint(pattern.transpose.map(_.mkString))
    horizontalReflectionPoints.appendedAll(verticalReflectionPoints)
  }

  def toggle(c: Char): Char = if (c == '.') '#' else '.'

  def removeSmudge(pattern: Pattern, smudge: SmudgePosition): Pattern =
    pattern.updated(smudge._1, pattern(smudge._1).updated(smudge._2, toggle(pattern(smudge._1)(smudge._2))))

  def findSmudges(pattern: Pattern): List[SmudgePosition] = {
    @tailrec
    def go(linesWithIds: List[(String, Int)], acc: List[SmudgePosition]): List[SmudgePosition] =
      linesWithIds match {
        case Nil => acc
        case _ :: Nil => acc
        case (headLine, headRow) :: tail =>
          go(tail, acc.appendedAll(
            tail
              .map(_._1)
              .map(_.zip(headLine).zipWithIndex.filter { case ((c1, c2), _) => c1 != c2 }.map(_._2))
              .filter(_.size == 1)
              .map(_.head)
              // (tail._2, columnIdOfCharDifference) is also a smudge, but they are identical, return only one
              .map(columnIdOfCharDifference => (headRow, columnIdOfCharDifference))
          ))
      }

    go(pattern.zipWithIndex, List.empty)
  }

  def removeSmudges(pattern: Pattern): Seq[Pattern] = {
    val horizontalSmudges = findSmudges(pattern)
    val verticalSmudges = findSmudges(pattern.transpose.map(_.mkString)).map(_.swap)
    val smudges = horizontalSmudges.appendedAll(verticalSmudges).distinct
    smudges.map(s => removeSmudge(pattern, s))
  }

  def task1(in: Seq[String]): Int = parsePatterns(in).map(p => patternSummaries(p).head).sum

  def task2(in: Seq[String]): Int =
    parsePatterns(in).flatMap(pattern => {
      val originalPatternSummary = patternSummaries(pattern).head
      removeSmudges(pattern).map(p => patternSummaries(p).filterNot(_ == originalPatternSummary))
        .filterNot(_.isEmpty).head
    }).sum
}
