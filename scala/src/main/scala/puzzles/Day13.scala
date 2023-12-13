package puzzles

import scala.annotation.tailrec

object Day13 {
  type Pattern = List[String]
  type SmudgePosition = (Int, Int)
  val emptyPattern: Pattern = List.empty[String]

  def findHorizontalReflectionPoints(pattern: Pattern): Seq[Int] =
    (1 until pattern.size).filter(idx =>
      (0 to Math.min(idx - 1, pattern.length - idx - 1))
        .forall(i => pattern(idx - i - 1) == pattern(idx + i)))

  def patternSummaries(pattern: Pattern): Seq[Int] = {
    val horizontalReflectionPoints = findHorizontalReflectionPoints(pattern).map(_ * 100)
    val verticalReflectionPoints = findHorizontalReflectionPoints(pattern.transpose.map(_.mkString))
    horizontalReflectionPoints.appendedAll(verticalReflectionPoints)
  }

  def indexOfDiffs(lines: (String, String)): Seq[Int] =
    lines._1.zip(lines._2).map { case (c1, c2) => c1 != c2 }.zipWithIndex.filter(_._1).map(_._2)

  def findSmudges(pattern: Pattern): List[SmudgePosition] = {
    @tailrec
    def go(pattern: Pattern, row: Int, acc: List[SmudgePosition]): List[SmudgePosition] =
      pattern match {
        case Nil => acc
        case _ :: Nil => acc
        case head :: tail =>
          go(tail, row + 1, acc.appendedAll({
            tail.map(indexOfDiffs(head, _)).filter(_.size == 1).map(_.head).map(col => (row, col))
          }))
      }

    go(pattern, 0, List.empty)
  }

  def toggle(c: Char): Char = if (c == '.') '#' else '.'

  def removeSmudge(pattern: Pattern, smudge: SmudgePosition): Pattern =
    pattern.updated(smudge._1, pattern(smudge._1).updated(smudge._2, toggle(pattern(smudge._1)(smudge._2))))

  def removeSmudges(pattern: Pattern): Seq[Pattern] = {
    val horizontalSmudges = findSmudges(pattern)
    val verticalSmudges = findSmudges(pattern.transpose.map(_.mkString)).map(_.swap)
    val smudges = horizontalSmudges.appendedAll(verticalSmudges).distinct
    smudges.map(s => removeSmudge(pattern, s))
  }

  def parsePatterns(in: Seq[String]): Seq[Pattern] = in.foldRight(Seq(emptyPattern)) {
    case (l, head :: tail) => l match {
      case l if l.isBlank => emptyPattern :: head :: tail
      case l => head.prepended(l) :: tail
    }
  }.filterNot(_.isEmpty)

  def task1(in: Seq[String]): Int = parsePatterns(in).map(p => patternSummaries(p).head).sum

  def task2(in: Seq[String]): Int = parsePatterns(in).flatMap(pattern => {
    val originalPatternSummary = patternSummaries(pattern).head
    removeSmudges(pattern).map(p => patternSummaries(p).filterNot(_ == originalPatternSummary))
      .filterNot(_.isEmpty).head
  }).sum
}
