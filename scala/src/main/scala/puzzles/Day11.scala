package puzzles

import scala.annotation.tailrec

object Day11 {

  type Image = List[List[Char]]
  type Coordinate = (Int, Int)
  type GalaxyPair = (Coordinate, Coordinate)

  def parseImage(in: Seq[String]): Image = in.map(_.toCharArray.toList).toList
  def findGalaxies(image: Image): List[Coordinate] = image.zipWithIndex
      .flatMap { case (l, r) => l.zipWithIndex.map { case (el, c) => (el, (r, c)) }}
      .filter(_._1 == '#').map(_._2)
  def pairGalaxies(galaxies: List[Coordinate]): List[GalaxyPair] = {
    @tailrec
    def go(galaxies: List[Coordinate], pairs: List[GalaxyPair]): List[GalaxyPair] =
      if (galaxies.size < 2) pairs else go(galaxies.tail, pairs.appendedAll(galaxies.tail.map((_, galaxies.head))))
    go(galaxies, List.empty)
  }
  def findRowsToBeExpanded(image: Image): List[Int] = image.zipWithIndex.filter(_._1.forall(_ == '.')).map(_._2)
  def findColumnsToBeExpanded(image: Image): List[Int] = findRowsToBeExpanded(image.transpose)
  def distanceBetween(pair: GalaxyPair, expandedRows: List[Int], expandedCols: List[Int], expansionLevel: Int): Long = {
    val (xMin, xMax) = (Math.min(pair._1._1, pair._2._1), Math.max(pair._1._1, pair._2._1))
    val (yMin, yMax) = (Math.min(pair._1._2, pair._2._2), Math.max(pair._1._2, pair._2._2))
    (xMax - xMin + yMax - yMin) +
      ((expandedRows.count(r => r > xMin && r < xMax) + expandedCols.count(c => c > yMin && c < yMax))
        * (expansionLevel - 1))
  }

  def task(image: Image, expansionLevel: Int): Long = {
    val expandedRows = findRowsToBeExpanded(image)
    val expandedCols = findColumnsToBeExpanded(image)
    pairGalaxies(findGalaxies(image))
      .map(pair => distanceBetween(pair, expandedRows, expandedCols, expansionLevel))
      .sum
  }

  def task1(in: Seq[String]): Long = task(parseImage(in), 2)
  def task2(in: Seq[String]): Long = task(parseImage(in), 1000000)
}
