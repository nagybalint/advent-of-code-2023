package puzzles

object Day16 {
  case class Pos(x: Int, y: Int) {
    def isValid(h: Int, w: Int): Boolean =
      x >= 0 && x < h && y >= 0 && y < w
  }

  object Direction extends Enumeration {
    type Direction = Value
    val Left = Value
    val Right = Value
    val Up = Value
    val Down = Value
  }

  case class Beam(pos: Pos, direction: Direction.Direction) {
    def move(direction: Direction.Direction): Beam = {
      direction match {
        case Direction.Left => Beam(pos.copy(y = pos.y - 1), direction)
        case Direction.Right => Beam(pos.copy(y = pos.y + 1), direction)
        case Direction.Up => Beam(pos.copy(x = pos.x - 1), direction)
        case Direction.Down => Beam(pos.copy(x = pos.x + 1), direction)
      }
    }
  }

  case class Tile(typ: Char) {
    def applyOnBeam(beam: Beam): Seq[Beam] = {
      typ match {
        case '.' => Seq(beam.move(beam.direction))
        case '-' =>
          beam.direction match {
            case d if d == Direction.Left || d == Direction.Right => Seq(beam.move(beam.direction))
            case _ => Seq(beam.move(Direction.Left), beam.move(Direction.Right))
          }
        case '|' =>
          beam.direction match {
            case d if d == Direction.Up || d == Direction.Down => Seq(beam.move(beam.direction))
            case _ => Seq(beam.move(Direction.Up), beam.move(Direction.Down))
          }
        case '/' =>
          beam.direction match {
            case Direction.Left => Seq(beam.move(Direction.Down))
            case Direction.Right => Seq(beam.move(Direction.Up))
            case Direction.Up => Seq(beam.move(Direction.Right))
            case Direction.Down => Seq(beam.move(Direction.Left))
          }
        case '\\' =>
          beam.direction match {
            case Direction.Left => Seq(beam.move(Direction.Up))
            case Direction.Right => Seq(beam.move(Direction.Down))
            case Direction.Up => Seq(beam.move(Direction.Left))
            case Direction.Down => Seq(beam.move(Direction.Right))
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
    val beam = Beam(Pos(0, 0), Direction.Right)
    energize(tiles, beam).size
  }

  def task2(in: Seq[String]): Int = {
    val tiles = parseTiles(in)
    val h = tiles.length
    val w = tiles.head.length
    val beamsEntering =
      ((0 until h).map(idx => Seq(Beam(Pos(idx, 0), Direction.Right), Beam(Pos(idx, w - 1), Direction.Left))) ++
        (0 until w).map(idy => Seq(Beam(Pos(0, idy), Direction.Down), Beam(Pos(h - 1, idy), Direction.Up)))).flatten
    beamsEntering.map(energize(tiles, _).size).max
  }
}
