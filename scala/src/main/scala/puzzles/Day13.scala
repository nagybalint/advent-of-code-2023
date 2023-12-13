package puzzles

import scala.annotation.tailrec

object Day13 {
  type Pattern = List[String]
  type SmudgePosition = (Int, Int)

  def findHorizontalReflectionPoint(pattern: Pattern): Seq[(Int, Int)] = {
    val patternArray = pattern.toArray // For fast indexing
    val m = pattern.zipWithIndex.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
    val candidates = m.filter(_._2.size > 1)
      .flatMap(_._2.sorted.sliding(2)).toSeq
      .map { case Seq(a, b) => (a, b) }
      .filter { case (a, b) => b - a == 1 }
    candidates.filter { case (a, b) =>
      val len = Math.min(a, patternArray.length - b - 1)
      (0 to len).forall(i => patternArray(a - i) == patternArray(b + i))
    }
  }

  def parsePatterns(in: Seq[String]): Seq[Pattern] = in.foldRight(Seq(List.empty[String])) { case (l, agg) => l match {
    case l if l.isBlank => agg.prepended(List.empty)
    case l => agg.updated(0, agg.head.prepended(l))
  }}

  def patternSummaries(pattern: Pattern): Seq[Int] = {
    val horizontalReflectionPoints = findHorizontalReflectionPoint(pattern).map(_._2 * 100)
    val verticalReflectionPoints = findHorizontalReflectionPoint(pattern.transpose.map(_.mkString)).map(_._2)
    horizontalReflectionPoints.appendedAll(verticalReflectionPoints)
  }

  def toggle(c: Char): Char = if (c == '.') '#' else '.'

  def removeSmudge(pattern: Pattern, smudge: SmudgePosition): Pattern =
    pattern.updated(smudge._1, pattern(smudge._1).updated(smudge._2, toggle(pattern(smudge._1)(smudge._2))))

  def smudgeRemover(pattern: Pattern): Seq[Pattern] = {
    def findSmudges(patternWithIds: List[(String, Int)]): List[SmudgePosition] = {
      @tailrec
      def go(patternWithIds: List[(String, Int)], acc: List[SmudgePosition]): List[SmudgePosition] =
        patternWithIds match {
          case Nil => acc
          case _ :: Nil => acc
          case (headLine, headRow) :: tail =>
            go(tail, acc.appendedAll(
                tail
                  .map(_._1)
                  .map(_.zip(headLine).zipWithIndex.filter { case ((c1, c2), _) => c1 != c2 }.map(_._2))
                  .filter(_.size == 1)
                  .map(x => (headRow, x.head))
              ))
        }
      go(patternWithIds, List.empty)
    }
    val horizontalSmudges = findSmudges(pattern.zipWithIndex)
    val verticalSmudges = findSmudges(pattern.transpose.map(_.mkString).zipWithIndex).map(_.swap)
    val smudges = horizontalSmudges.appendedAll(verticalSmudges).distinct
    smudges.map(s => removeSmudge(pattern, s))
  }

  def task1(in: Seq[String]): Int = parsePatterns(in).map(p => patternSummaries(p).head).sum
  def task2(in: Seq[String]): Int =
    parsePatterns(in).flatMap(pattern => {
      val originalPatternSummary = patternSummaries(pattern).head
      smudgeRemover(pattern).map(p => patternSummaries(p).filterNot(_ == originalPatternSummary)).filterNot(_.isEmpty).head
    }).sum
}
