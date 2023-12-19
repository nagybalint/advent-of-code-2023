package puzzles

object Day16 {
  case class Pos(x: Int, y: Int) {
    def isValid(h: Int, w: Int): Boolean =
      x >= 0 && x < h && y >= 0 && y < w
  }

  case class Beam(pos: Pos, direction: String) {
    def move(direction: String): Beam = {
      direction match {
        case "L" => Beam(pos.copy(y = pos.y - 1), direction)
        case "R" => Beam(pos.copy(y = pos.y + 1), direction)
        case "U" => Beam(pos.copy(x = pos.x - 1), direction)
        case "D" => Beam(pos.copy(x = pos.x + 1), direction)
      }
    }
  }

  case class Tile(typ: Char) {
    def applyOnBeam(beam: Beam): Seq[Beam] = {
      typ match {
        case '.' => Seq(beam.move(beam.direction))
        case '-' =>
          beam.direction match {
            case d if d == "L" || d == "R" => Seq(beam.move(beam.direction))
            case _ => Seq(beam.move("L"), beam.move("R"))
          }
        case '|' =>
          beam.direction match {
            case d if d == "U" || d == "D" => Seq(beam.move(beam.direction))
            case _ => Seq(beam.move("U"), beam.move("D"))
          }
        case '/' =>
          beam.direction match {
            case "L" => Seq(beam.move("D"))
            case "R" => Seq(beam.move("U"))
            case "U" => Seq(beam.move("R"))
            case "D" => Seq(beam.move("L"))
          }
        case '\\' =>
          beam.direction match {
            case "L" => Seq(beam.move("U"))
            case "R" => Seq(beam.move("D"))
            case "U" => Seq(beam.move("L"))
            case "D" => Seq(beam.move("R"))
          }
      }
    }
  }

  def energize(tiles: Array[Array[Tile]], beamEntering: Beam): Set[Pos] = {
    val h = tiles.length
    val w = tiles.head.length
    val memo = collection.mutable.HashSet.empty[Beam]
    @scala.annotation.tailrec
    def go(beams: Seq[Beam]): Unit = {
      val nextTiles = beams.filterNot(memo.contains).flatMap(b => {
        memo.addOne(b)
        tiles(b.pos.x)(b.pos.y).applyOnBeam(b).filter(_.pos.isValid(h, w))
      })
      if (nextTiles.nonEmpty) {
        go(nextTiles)
      }
    }

    go(Seq(beamEntering))
    memo.map(_.pos).toSet
  }

  def parseTiles(in: Seq[String]): Array[Array[Tile]] = in.map(_.map(Tile).toArray).toArray

  def task1(in: Seq[String]): Int = {
    val tiles = parseTiles(in)
    val beam = Beam(Pos(0, 0), "R")
    energize(tiles, beam).size
  }

  def task2(in: Seq[String]): Int = {
    val tiles = parseTiles(in)
    val h = tiles.length
    val w = tiles.head.length
    val beamsEntering =
      ((0 until h).map(idx => Seq(Beam(Pos(idx, 0), "R"), Beam(Pos(idx, w - 1), "L"))) ++
        (0 until w).map(idy => Seq(Beam(Pos(0, idy), "D"), Beam(Pos(h - 1, idy), "U")))).flatten
    beamsEntering.map(energize(tiles, _).size).max
  }
}
