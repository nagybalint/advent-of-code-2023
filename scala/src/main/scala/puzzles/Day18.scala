package puzzles

object Day18 {
  case class Pos(x: Long, y: Long)
  case class Instr(dir: String, amt: Int, color: String) {
    def getHexEncoded: Instr = Instr(color.takeRight(1), Integer.parseInt(color.take(5), 16), "")
  }

  def vertices(instructions: Seq[Instr]): Seq[Pos] =
    instructions.foldLeft(Seq(Pos(0, 0))) { case (vertices, i) =>
      vertices.appended(i.dir match {
        case d if (d == "U" || d == "3") => vertices.last.copy(x = vertices.last.x - i.amt)
        case d if (d == "D" || d == "1") => vertices.last.copy(x = vertices.last.x + i.amt)
        case d if (d == "L" || d == "2") => vertices.last.copy(y = vertices.last.y - i.amt)
        case d if (d == "R" || d == "0") => vertices.last.copy(y = vertices.last.y + i.amt)
      })
    }.dropRight(1)

  def shoelaceFormula(vertices: Seq[Pos]): Long =
    Math.abs(vertices.appended(vertices.head).sliding(2).foldLeft(0L) {
      case (a, List(v1, v2)) => a + v1.x * v2.y - v1.y * v2.x
    }) / 2

  def parseInstructions(in: Seq[String]): Seq[Instr] = in.map { case s"$d $a (#$c)" => Instr(d, a.toInt, c) }
  def pickTheorem(internalPoints: Long, boundaryPoints: Long): Long = internalPoints + boundaryPoints / 2 + 1
  def task(inst: Seq[Instr]): Long = pickTheorem(shoelaceFormula(vertices(inst)), inst.map(_.amt).sum)
  def task1(in: Seq[String]): Long = task(parseInstructions(in))
  def task2(in: Seq[String]): Long = task(parseInstructions(in).map(_.getHexEncoded))
}
