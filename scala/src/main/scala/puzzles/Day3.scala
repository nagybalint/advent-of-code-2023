package puzzles

object Day3 {

  case class PartNumber(number: Int, row: Int, leftBound: Int, rightBound: Int) {
    def isAdjacentTo(x: Int, y: Int): Boolean =
      (x >= row - 1 && x <= row + 1) && (y <= rightBound + 1 && y >= leftBound - 1)
  }
  case class Symbol(value: String, row: Int, col: Int)
  case class Engine(partNumbers: Seq[PartNumber], symbols: Seq[Symbol])

  object Engine {
    def empty: Engine = Engine(Seq.empty, Seq.empty)
  }

  private def isPartValid(part: PartNumber, symbolsToCheck: Seq[Symbol]): Boolean =
    symbolsToCheck.exists(symbol => part.isAdjacentTo(symbol.row, symbol.col))

  private def parseSchematic(schematic: Seq[String]): Engine =
    schematic.zipWithIndex.foldLeft(Engine.empty) { case (engine, (line, rowId)) =>
      line.zipWithIndex.foldLeft[(Engine, Option[PartNumber])](
        engine, None
      ) {
        case ((engine, currentPart), (nextCh, colId)) =>
          if (nextCh.isDigit) {
            val nextDigit = nextCh.toString.toInt
            val part = currentPart match {
              case Some(value) => value.copy(number = value.number * 10 + nextDigit, rightBound = colId)
              case None => PartNumber(number = nextDigit, row = rowId, leftBound = colId, rightBound = colId)
            }
            (engine, Some(part))
          } else {
            val parts = currentPart match {
              case Some(value) => engine.partNumbers.appended(value)
              case None => engine.partNumbers
            }
            val symbols = nextCh match {
              case '.' => engine.symbols
              case c => engine.symbols.appended(Symbol(value = c.toString, row = rowId, col = colId))
            }
            (Engine(parts, symbols), None)
          }
      } match {
        case (engine, Some(lastPart)) => engine.copy(partNumbers = engine.partNumbers.appended(lastPart))
        case (engine, None) => engine
      }
    }

  def task1(schematic: Seq[String]): Int = {
    val engine = parseSchematic(schematic)
    engine.partNumbers
      .filter(part => isPartValid(part, engine.symbols))
      .map(_.number).sum
  }

  def task2(schematic: Seq[String]): Int = {
    val engine = parseSchematic(schematic)
    engine.symbols
      .filter(_.value == "*")
      .map(star => engine.partNumbers.filter(_.isAdjacentTo(star.row, star.col)))
      .filter(_.size == 2)
      .map(_.map(_.number).product)
      .sum
  }
}
