package puzzles

object Day12 {

  def getValidOptions(springs: Seq[Char], damagedDist: Seq[Int], dotsToBeInserted: Int): Long = {
    val helper = collection.mutable.Map.empty[(Seq[Char], Seq[Int], Int), Long]
    def go(springs: Seq[Char], damagedDist: Seq[Int], dotsToBeInserted: Int): Long = {
      helper.get((springs, damagedDist, dotsToBeInserted)) match {
        case Some(value) => value
        case None =>
          val result = springs.headOption match {
            case None => if (damagedDist.isEmpty && dotsToBeInserted == 0) 1 else 0
            case Some('.') => go(springs.tail, damagedDist, dotsToBeInserted)
            case Some('#') =>
              damagedDist.headOption match {
                case Some(damagedAmt) =>
                  if (springs.take(damagedAmt).contains('.')) {
                    0
                  } else {
                    val remaining = springs.drop(damagedAmt)
                    remaining.headOption match {
                      case Some('#') => 0
                      case Some('.') => go(remaining.tail, damagedDist.tail, dotsToBeInserted)
                      case Some('?') => if (dotsToBeInserted > 0) go(remaining.tail, damagedDist.tail, dotsToBeInserted - 1) else 0
                      case None => go(remaining, damagedDist.tail, dotsToBeInserted)
                    }
                  }
                case None => 0
              }
            case Some('?') =>
              val a = go(springs.updated(0, '#'), damagedDist, dotsToBeInserted)
              val b = if (dotsToBeInserted > 0) go(springs.tail, damagedDist, dotsToBeInserted - 1) else 0
              a + b
          }
          helper.addOne((springs, damagedDist, dotsToBeInserted), result)
          result
      }
    }
    go(springs, damagedDist, dotsToBeInserted)
  }

  def task(in: Seq[String], foldLevel: Int): Long = in.map(line => {
      val Seq(rawStrings, rawDamagedDist) = line.split(" ").toSeq
      val springs = (1 to foldLevel)
        .flatMap(_ => rawStrings.toCharArray.appended('?'))
        .dropRight(1).dropWhile(_ == '.').reverse.dropWhile(_ == '.').reverse
      val cleanedSprings = springs
        .sliding(2).flatMap { case Seq(a, b) => if (a == '.' && b == '.') Seq.empty else Seq(a) }
        .toSeq.appended(springs.last)
      val damaged = (1 to foldLevel).flatMap(_ => rawDamagedDist.split(",")).map(_.toInt)
      val unknownIds = cleanedSprings.zipWithIndex.filter(_._1 == '?').map(_._2)
      val damagedToBeInserted = damaged.sum - cleanedSprings.count(_ == '#')
      val dotsToBeInserted = unknownIds.size - damagedToBeInserted
      getValidOptions(cleanedSprings, damaged, dotsToBeInserted)
    }).sum

  def task1(in: Seq[String]): Long = task(in, 1)
  def task2(in: Seq[String]): Long = task(in, 5)
}
