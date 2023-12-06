package puzzles

case class Race(time: Long, record: Long) {
  def score(timePressed: Long): Long = (time - timePressed) * timePressed
  def isWinning(score: Long): Boolean = score > record
  def getWaysToBeatRace: Long = (0L to time).map(timePressed => score(timePressed)).count(score => isWinning(score))
}

object Day6 {
  def parseRaces(dataLines: Seq[String]): Seq[Race] = {
    val times = dataLines.head.split(" ").filterNot(_.isBlank).drop(1).map(_.toLong)
    val records = dataLines(1).split(" ").filterNot(_.isBlank).drop(1).map(_.toLong)
    times.zip(records).map { case (time, record) => Race(time, record)}
  }

  def parseSingleRace(dataLines: Seq[String]): Race = {
    val time = dataLines.head.split(" ").filterNot(_.isBlank).drop(1).mkString.toLong
    val record = dataLines(1).split(" ").filterNot(_.isBlank).drop(1).mkString.toLong
    Race(time, record)
  }

  def task1(dataLines: Seq[String]): Long = parseRaces(dataLines).map(_.getWaysToBeatRace).product
  // Ideally this should be done by calculating the roots of a quadratic function, but as brute force works, ¯\_(ツ)_/¯ 
  def task2(dataLines: Seq[String]): Long = parseSingleRace(dataLines).getWaysToBeatRace
}
