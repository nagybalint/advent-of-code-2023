package puzzles

object Day13 {
  type Pattern = List[String]
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

  def smudgeRemover(pattern: Pattern): Seq[Pattern] = {
    def findSmudgeForHead(patternWithIds: List[(String, Int)]): Seq[(Int, Int)] = {
      if (patternWithIds.size < 2) {
        Seq.empty
      } else {
        val (headLine, headRow) = patternWithIds.head
        val toSmudge = patternWithIds.tail.map(_._1)
          .flatMap(_.zip(headLine).zipWithIndex.filter {
            case ((c1, c2), _) => c1 != c2 } map(_._2) match {
              case diffCharIds if diffCharIds.size == 1 => Seq((headRow, diffCharIds.head))
              case _ => Seq.empty
            })
        toSmudge.appendedAll(findSmudgeForHead(patternWithIds.tail))
        }
    }
    def toggle(c: Char): Char = if (c == '.') '#' else '.'
    def removeSmudge(smudge: (Int, Int)): Pattern = {
      pattern.updated(smudge._1, pattern(smudge._1).updated(smudge._2, toggle(pattern(smudge._1)(smudge._2))))
    }
    val horizontalSmudges = findSmudgeForHead(pattern.zipWithIndex)
    val verticalSmudges = findSmudgeForHead(pattern.transpose.map(_.mkString).zipWithIndex).map(_.swap)
    val smudges = horizontalSmudges.appendedAll(verticalSmudges).distinct
    smudges.map(removeSmudge)
  }

  def task1(in: Seq[String]): Int = parsePatterns(in).map(p => patternSummaries(p).head).sum
  def task2(in: Seq[String]): Int = {
    val aa = parsePatterns(in)
      .map(pattern => {
        val originalPatternSummary = patternSummaries(pattern).head
        smudgeRemover(pattern).map(p => patternSummaries(p).filterNot(_ == originalPatternSummary)).filterNot(_.isEmpty).head
      })
  aa.flatten.sum
  }
}
