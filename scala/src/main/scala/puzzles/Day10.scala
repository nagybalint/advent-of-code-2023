package puzzles

import scala.annotation.tailrec

object Day10 {
  case class Coordinate(x: Int, y: Int)
  case class Tile(label: Char, pos: Coordinate) {
    def allowsNorthStep: Boolean = Seq('S', '|', 'J', 'L').contains(label)
    def allowsSouthStep: Boolean = Seq('S', '|', '7', 'F').contains(label)
    def allowsWestStep: Boolean = Seq('S', '-', 'J', '7').contains(label)
    def allowsEastStep: Boolean = Seq('S', 'F', '-', 'L').contains(label)

    def getNorthNeighbor(map: Seq[Seq[Tile]]): Option[Tile] =
      if (pos.x == 0) None else Some(map(pos.x - 1)(pos.y))
    def getSouthNeighbor(map: Seq[Seq[Tile]]): Option[Tile] =
      if (pos.x == map.size - 1) None else Some(map(pos.x + 1)(pos.y))
    def getWestNeighbor(map: Seq[Seq[Tile]]): Option[Tile] =
      if (pos.y == 0) None else Some(map(pos.x)(pos.y - 1))
    def getEastNeighbor(map: Seq[Seq[Tile]]): Option[Tile] =
      if (pos.y == map.head.size - 1) None else Some(map(pos.x)(pos.y + 1))

    def getWaysOut(map: Seq[Seq[Tile]]): Seq[Tile] = {
      val maybeNorth = getNorthNeighbor(map) match {
        case Some(n) if n.allowsSouthStep && allowsNorthStep => Some(n)
        case _ => None
      }
      val maybeSouth = getSouthNeighbor(map) match {
        case Some(n) if n.allowsNorthStep && allowsSouthStep => Some(n)
        case _ => None
      }
      val maybeEast = getEastNeighbor(map) match {
        case Some(n) if n.allowsWestStep && allowsEastStep => Some(n)
        case _ => None
      }
      val maybeWest = getWestNeighbor(map) match {
        case Some(n) if n.allowsEastStep && allowsWestStep => Some(n)
        case _ => None
      }
      Seq(maybeNorth, maybeSouth, maybeEast, maybeWest).filter(_.isDefined).map(_.get)
    }
  }

  def parseMap(in: Seq[String]): Seq[Seq[Tile]] = in.zipWithIndex.map {
    case (str, row) => str.toCharArray.toSeq.zipWithIndex.map {
      case (ch, col) => Tile(ch, Coordinate(row, col))
    }
  }
  def getStartPos(map: Seq[Seq[Tile]]): Tile = map.flatMap(_.find(_.label == 'S')).head

  def findLoop(map: Seq[Seq[Tile]], startPos: Tile): Seq[Tile] = {
    @tailrec
    def go(t: Tile, steps: Seq[Tile]): Seq[Tile] = {
      val waysOut = t.getWaysOut(map)
      val nextStep: Tile = if (steps.isEmpty) waysOut.head else waysOut.find(_ != steps.last).get
      if (nextStep == startPos) steps.appended(t) else go(nextStep, steps.appended(t))
    }
    go(startPos, Seq.empty)
  }

  def task1(in: Seq[String]): Int = {
    val map = parseMap(in)
    val startPos = getStartPos(map)
    val steps = findLoop(map, startPos)
    steps.size / 2
  }

  def isUTurn(segment: Seq[Tile]): Boolean = {
    val firstLabel = segment.head.label
    val lastLabel = segment.last.label
    (firstLabel == 'L' && lastLabel == 'J') || (firstLabel == 'F' && lastLabel == '7')
  }

  private def splitToSegments(crossSection: Seq[Tile]): Seq[Seq[Tile]] =
    crossSection.sortBy(_.pos.y).filterNot(_.label == '-').foldLeft(Seq.empty[Seq[Tile]]) { case (tileGroups, tile) =>
      if (!tile.allowsWestStep)
        tileGroups.appended(Seq(tile))
      else
        tileGroups.dropRight(1).appended(tileGroups.last.appended(tile))
    }

  private def countTilesEnclosedByCrossSection(segments: Seq[Seq[Tile]]): Int =
    segments
      .sliding(2)
      .map(s => (s.head, s.last))
      .foldLeft(
        (false, 0)
      ) {
        case ((wasInsideLoop, totalEnclosed), (segment1, segment2)) =>
          val isInsideLoop = if (isUTurn(segment1)) wasInsideLoop else !wasInsideLoop
          val enclosedBySegments = if (isInsideLoop) segment2.head.pos.y - segment1.last.pos.y - 1 else 0
          (isInsideLoop, totalEnclosed + enclosedBySegments)
      }._2

  def replaceStart(start: Tile, neighbors: Seq[Tile]): Tile = {
    neighbors.partition(_.pos.x == start.pos.x) match {
      case (inline, _) if inline.size == 2 => start.copy(label = '-')
      case (inline, _) if inline.isEmpty => start.copy(label = '|')
      case (Seq(inline), Seq(otherLine)) => {
        if (inline.pos.y < start.pos.y) {
          if (otherLine.pos.x < start.pos.x) start.copy(label = 'J')
          else start.copy(label = '7')
        } else {
          if (otherLine.pos.x < start.pos.x) start.copy(label = 'L')
          else start.copy(label = 'F')
        }
      }
    }
  }

  def task2(in: Seq[String]): Int = {
    val map = parseMap(in)
    val startPos = getStartPos(map)
    val steps = findLoop(map, startPos)
    val crossSections = steps
      .updated(0, replaceStart(steps.head, Seq(steps(1), steps.last)))
      .groupBy(_.pos.x)
      .values
      .toSeq
      .sortBy(_.head.pos.x)
    crossSections
      .map(splitToSegments)
      .map(countTilesEnclosedByCrossSection)
      .sum
  }
}
