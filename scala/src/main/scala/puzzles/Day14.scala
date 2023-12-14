package puzzles

object Day14 {
  type Platform = List[List[Char]]

  def splitRocks(rocks: List[Char]): List[List[Char]] = {
    @annotation.tailrec
    def go(rocks: List[Char], currentGroup: List[Char], groups: List[List[Char]]): List[List[Char]] =
      rocks match {
        case Nil => groups :+ currentGroup
        case '#' :: rest => go(rest, Nil, groups :+ currentGroup)
        case ch :: rest => go(rest, currentGroup :+ ch, groups)
      }
    go(rocks, Nil, Nil)
  }

  def tiltWest(p: Platform): Platform = p.map(splitRocks(_).map(_.sorted.reverse).reduce(_ :+ '#' :++ _))
  def tiltEast(p: Platform): Platform = tiltWest(p.map(_.reverse)).map(_.reverse)
  def tiltNorth(p: Platform): Platform = tiltWest(p.transpose).transpose
  def tiltSouth(p: Platform): Platform = tiltNorth(p.reverse).reverse

  def spin(platform: Platform, cycles: Int): Platform = {
    val cache = collection.mutable.HashMap.empty[Platform, Int]
    @annotation.tailrec
    def go(platform: Platform, current: Int): Platform =
      if (current == cycles) platform
      else cache.put(platform, current) match {
        case Some(start) => cache.map(_.swap).getOrElse((cycles - current) % (current - start) + start, platform)
        case None => go(tiltEast(tiltSouth(tiltWest(tiltNorth(platform)))), current + 1)
      }
    go(platform, 0)
  }

  def parsePlatform(in: Seq[String]): Platform = in.map(_.toCharArray.toList).toList
  def getLoad(p: Platform): Int = p.transpose.map(_.reverse.zipWithIndex.filter(_._1 == 'O').map(_._2 + 1).sum).sum
  def task1(in: Seq[String]): Int = getLoad(tiltNorth(parsePlatform(in)))
  def task2(in: Seq[String]): Int = getLoad(spin(parsePlatform(in), 1000000000))
}
