package puzzles

object Day12 {

  def getValidOptions(springs: Seq[Char], damagedDist: Seq[Int], operationalToBeInserted: Int): Long = {
    val memo = collection.mutable.Map.empty[(Int, Int), Long]
    def go(springs: Seq[Char], damagedDist: Seq[Int], operationalToBeInserted: Int): Long = {
      val partialResult = memo.get((springs.size, operationalToBeInserted)) match {
        case Some(value) => value
        case None =>
          springs.headOption match {
            case None => if (damagedDist.isEmpty && operationalToBeInserted == 0) 1 else 0
            case Some('.') => go(springs.tail, damagedDist, operationalToBeInserted)
            case Some('#') =>
              damagedDist.headOption match {
                case Some(hasToBeDamaged) =>
                  if (springs.take(hasToBeDamaged).contains('.')) 0
                  else
                    springs.drop(hasToBeDamaged) match {
                      case '#' :: _ => 0
                      case '.' :: remaining => go(remaining, damagedDist.tail, operationalToBeInserted)
                      case '?' :: remaining =>
                        if (operationalToBeInserted > 0) go(remaining, damagedDist.tail, operationalToBeInserted - 1)
                        else 0
                      case Nil => go(Nil, damagedDist.tail, operationalToBeInserted)
                    }
                case None => 0
              }
            case Some('?') =>
              val a = go(springs.updated(0, '#'), damagedDist, operationalToBeInserted)
              val b = if (operationalToBeInserted > 0) go(springs.tail, damagedDist, operationalToBeInserted - 1) else 0
              a + b
          }
      }
      memo.update((springs.size, operationalToBeInserted), partialResult)
      partialResult
    }

    go(springs, damagedDist, operationalToBeInserted)
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
