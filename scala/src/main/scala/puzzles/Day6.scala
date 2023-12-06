package puzzles

case class Race(time: Long, record: Long) {
  def score(timePressed: Long) = {
    val speed = timePressed
    val goTime = time - timePressed
    (time - timePressed) * speed
  }
  def isWinning(score: Long) = score > record
  def getWaysToBeatRace: Long = (0L to time).map(timePressed => score(timePressed))
    .count(score => isWinning(score))
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
  def task1(dataLines: Seq[String]): Long = {
    val races = parseRaces(dataLines)
    races.map(_.getWaysToBeatRace).product
  }

  def task2(dataLines: Seq[String]): Long = {
    val race = parseSingleRace(dataLines)
    race.getWaysToBeatRace
  }
}
