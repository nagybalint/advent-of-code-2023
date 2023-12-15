package puzzles

object Day15 {
  type Boxes = Map[Int, Seq[(String, Int)]]

  def hash(in: String): Int = in.toCharArray.foldLeft(0) { case (current, ch) => ((current + ch.toInt) * 17) % 256 }

  def processInstruction(boxes: Boxes, instruction: String): Boxes =
    instruction match {
      case s"${l}=${fp}" => boxes.updatedWith(hash(l)) {
        case Some(v) => Some(v.map(x => if (x._1 == l) (l, fp.toInt) else x).appended((l, fp.toInt)).distinct)
        case None => Some(Seq((l, fp.toInt)))
      }
      case s"${l}-" => boxes.updatedWith(hash(l))(_.map(_.filterNot(_._1 == l)))
    }

  def focusingPower(boxes: Boxes): Int =
    boxes.map { case (k, v) => v.zipWithIndex.map { case ((_, i), idx) => (1 + k) * (idx + 1) * i }.sum }.sum

  def task1(in: Seq[String]): Int = in.head.split(",").map(hash).sum
  def task2(in: Seq[String]): Int = focusingPower(
    in.head.split(",").foldLeft(Map.empty.asInstanceOf[Boxes])(processInstruction)
  )
}
