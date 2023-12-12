package puzzles

import scala.util.matching.Regex

object Day12 {

  def task1(in: Seq[String]): Int = {
    val ccc = in.map(i => {
      val asd = i.split(" ")
      val springs = asd.head.toCharArray
      val damaged = asd.last.split(",")
      val damagedAmt = damaged.map(_.toInt).sum
      val damagedRegex: Regex = ("\\.*" + damaged.map(s => s"#{$s}").mkString("\\.+") + "\\.*").r
      val damagedToBeInserted = damagedAmt - springs.count(_ == '#')
      val unknownIds = springs.zipWithIndex.filter(_._1 == '?').map(_._2)
      val damageInsertLocationOptions = unknownIds.combinations(damagedToBeInserted)
      val possibilities = damageInsertLocationOptions.map(opt => {
        opt.foldLeft(springs) { case (sp, idx) => sp.updated(idx, '#') }.mkString.replace('?', '.')
      })
      val validOptions = possibilities.count(p => damagedRegex.matches(p))
      validOptions
    })
    val ccs = ccc.sum
    ccs
  }

  def getValidOptions(sss: Seq[Char], dddd: Seq[Int], didid: Int): Long = {
    var m = collection.mutable.Map.empty[(Seq[Char], Seq[Int], Int), Long]

    def go(springs: Seq[Char], damagedDist: Seq[Int], dotsToBeInserted: Int): Long = {
      m.get((springs, damagedDist, dotsToBeInserted)) match {
        case Some(value) => value
        case None =>
          val res = springs.headOption match {
            case None => if (damagedDist.isEmpty && dotsToBeInserted == 0) 1 else 0
            case Some('.') =>
              go(springs.tail, damagedDist, dotsToBeInserted)
            case Some('#') =>
              damagedDist.headOption match {
                case Some(d) =>
                  if (springs.take(d).contains('.')) {
                    0
                  } else {
                    val remaining = springs.drop(d)
                    remaining.headOption match {
                      case Some('#') =>
                        0
                      case Some('.') =>
                        go(remaining.tail, damagedDist.tail, dotsToBeInserted)
                      case Some('?') =>
                        if (dotsToBeInserted > 0) {
                          go(remaining.tail, damagedDist.tail, dotsToBeInserted - 1)
                        } else {
                          0
                        }
                      case None =>
                        go(remaining, damagedDist.tail, dotsToBeInserted)
                    }
                  }
                case None =>
                  0
              }
            case Some('?') =>
              val a = go(springs.updated(0, '#'), damagedDist, dotsToBeInserted)
              val b = if (dotsToBeInserted > 0) {
                go(springs.tail, damagedDist, dotsToBeInserted - 1)
              } else {
                0
              }
              a + b
          }
          m.addOne((springs, damagedDist, dotsToBeInserted), res)
          res
      }
    }
      go(sss, dddd, didid)
    }
  def task2(in: Seq[String]): Long = {
    val ccc = in.map(i => {
      val asd = i.split(" ")
      val rawString = asd.head.toCharArray
      val rawDamaged = asd.last.split(",")
      val springs = (1 to 5)
        .flatMap(_ => rawString.appended('?'))
        .dropRight(1).dropWhile(_ == '.').reverse.dropWhile(_ == '.').reverse
      val cleanedSprings = springs
        .sliding(2).flatMap { case Seq(a, b) => if (a == '.' && b == '.') Seq.empty else Seq(a) }
        .toSeq.appended(springs.last)
      val damaged = (1 to 5).flatMap(_ => rawDamaged).map(_.toInt)
      val unknownIds = cleanedSprings.zipWithIndex.filter(_._1 == '?').map(_._2)
      val damagedToBeInserted = damaged.sum - cleanedSprings.count(_ == '#')
      val dotsToBeInserted = unknownIds.size - damagedToBeInserted
      val res = getValidOptions(cleanedSprings, damaged, dotsToBeInserted)
      res
      }
    )
    ccc.sum
  }
}
