package puzzles

object Day12 {

  def getValidOptions(springs: Seq[Char], damagedDist: Seq[Int], operationalToBeInserted: Int): Long = {
    type Memo = Map[(Seq[Char], Seq[Int], Int), Long]

    def go(springs: Seq[Char], damagedDist: Seq[Int], operationalToBeInserted: Int, memo: Memo): (Long, Memo) = {
      def resultWithUpdatedMemo(h: Memo, v: Long): (Long, Memo) =
        (v, h.updated((springs, damagedDist, operationalToBeInserted), v))

      memo.get((springs, damagedDist, operationalToBeInserted)) match {
        case Some(value) => (value, memo)
        case None =>
          springs.headOption match {
            case None =>
              resultWithUpdatedMemo(memo, if (damagedDist.isEmpty && operationalToBeInserted == 0) 1L else 0L)
            case Some('.') => go(springs.tail, damagedDist, operationalToBeInserted, memo)
            case Some('#') =>
              damagedDist.headOption match {
                case Some(hasToBeDamaged) =>
                  if (springs.take(hasToBeDamaged).contains('.')) {
                    resultWithUpdatedMemo(memo, 0L)
                  } else {
                    springs.drop(hasToBeDamaged) match {
                      case '#' :: _ => resultWithUpdatedMemo(memo, 0L)
                      case '.' :: remaining => go(remaining, damagedDist.tail, operationalToBeInserted, memo)
                      case '?' :: remaining =>
                        if (operationalToBeInserted > 0)
                          go(remaining, damagedDist.tail, operationalToBeInserted - 1, memo)
                        else resultWithUpdatedMemo(memo, 0L)
                      case Nil => go(Nil, damagedDist.tail, operationalToBeInserted, memo)
                    }
                  }
                case None => resultWithUpdatedMemo(memo, 0L)
              }
            case Some('?') =>
              val (a, h) = go(springs.updated(0, '#'), damagedDist, operationalToBeInserted, memo)
              val (b, h2) = if (operationalToBeInserted > 0)
                go(springs.tail, damagedDist, operationalToBeInserted - 1, h)
              else resultWithUpdatedMemo(h, 0L)
              resultWithUpdatedMemo(h2, a + b)
          }
      }
    }

    go(springs, damagedDist, operationalToBeInserted, Map.empty.asInstanceOf[Memo])._1
  }

  def task(dataLines: Seq[String], folds: Int): Long = dataLines.map(line => {
    val Seq(rawStrings, rawDamagedDist) = line.split(" ").toSeq
    val unFoldedSprings = (1 to folds)
      .flatMap(_ => rawStrings.toCharArray.appended('?'))
      .dropRight(1).dropWhile(_ == '.').reverse.dropWhile(_ == '.').reverse
    val cleanedSprings = unFoldedSprings
      .sliding(2).flatMap { case Seq(a, b) => if (a == '.' && b == '.') Seq.empty else Seq(a) }
      .toSeq.appended(unFoldedSprings.last)
    val damaged = (1 to folds).flatMap(_ => rawDamagedDist.split(",")).map(_.toInt)
    val unknown = cleanedSprings.count(_ == '?')
    val damagedToBeInserted = damaged.sum - cleanedSprings.count(_ == '#')
    val operationalToBeInserted = unknown - damagedToBeInserted
    getValidOptions(cleanedSprings, damaged, operationalToBeInserted)
  }).sum

  def task1(dataLines: Seq[String]): Long = task(dataLines, 1)
  def task2(dataLines: Seq[String]): Long = task(dataLines, 5)
}
