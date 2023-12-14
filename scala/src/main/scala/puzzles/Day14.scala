package puzzles

object Day14 {
  type Platform = List[List[Char]]

  def getLoadOnPlatform(platform: Platform): Int =
    platform.transpose.map(_.reverse.zipWithIndex.filter(_._1 == 'O').map(_._2 + 1).sum).sum

  def tiltWest(platform: Platform): Platform = {
    val segmentedPlatform: List[List[List[Char]]] = platform.map(row => {
      val segmentBounds = row.zipWithIndex.filter(_._1 == '#').map(_._2).prepended(-1).appended(row.length)
      val segments = segmentBounds.sliding(2).map { case List(a, b) => {
        if (b - a == 1) List.empty[Char]
        else row.slice(a + 1 , b)
      }}.toList
      segments
    })
    segmentedPlatform.map(_.map(_.sorted.reverse.mkString).mkString("#").toCharArray.toList)
  }

  def tiltEast(platform: Platform): Platform = tiltWest(platform.map(_.reverse)).map(_.reverse)
  def tiltNorth(platform: Platform): Platform = tiltWest(platform.transpose).transpose
  def tiltSouth(platform: Platform): Platform = tiltNorth(platform.reverse).reverse

  val m = collection.mutable.HashMap.empty[Platform, Int]

  def spinCycle(platform: Platform): Platform =
      tiltEast(tiltSouth(tiltWest(tiltNorth(platform))))

  def getFinalState(loopBeganAt: Int, loopRealisedAt: Int, totalSpins: Int): Platform = {
    val loopLength = loopRealisedAt - loopBeganAt
    val spinsRemaining = totalSpins - loopRealisedAt
    val cycleOfLastState = spinsRemaining % loopLength + loopBeganAt
    val lastState = m.find(_._2 == cycleOfLastState).get._1
    lastState
  }

  def spin(platform: List[List[Char]], amt: Int): Platform = {
    m.addOne(platform, 0)
    (1 to amt).foldLeft(platform) { case (p, i) =>
      val spinned = spinCycle(p)
      m.get(spinned) match {
        case Some(cycleBeganAt) =>
          return getFinalState(cycleBeganAt, i, amt)
        case None =>
          m.update(spinned, i)
          spinned
      }
    }
  }

  def task1(in: Seq[String]): Int = {
    val platform = in.map(_.toCharArray.toList).toList
    getLoadOnPlatform(tiltNorth(platform))
  }

  def task2(in: Seq[String]): Int = {
    val platform = in.map(_.toCharArray.toList).toList
    val spinned = spin(platform, 1000000000)
    getLoadOnPlatform(spinned)
  }

}
