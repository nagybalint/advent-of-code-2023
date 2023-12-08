package puzzles

import scala.annotation.tailrec

object Day8 {

  private def parseInstructions(dataLine: String): Seq[String] = dataLine.toCharArray.map(_.toString)
  private def parseNodes(dataLines: Seq[String]): Map[String, (String, String)] =
    dataLines.foldLeft(Map.empty[String, (String, String)]) { case (m, node) => node match {
      case s"$from = ($l, $r)" => m.updated(from, (l, r))
    }}

  def getToGoal(
    instructions: Seq[String],
    nodes: Map[String, (String, String)],
    startingNode: String,
    goalEval: String => Boolean,
    startingId: Int = 0,
    idEval: Int => Boolean = _ => true
  ): (String, Int) = {
    @tailrec
    def go(step: String, id: Int): (String, Int) = {
      if (goalEval(step) && idEval(id)) (step, id)
      else go(instructions(id % instructions.size) match {
          case "L" => nodes(step)._1
          case "R" => nodes(step)._2
        }, id + 1)
    }
    val (goal, id) = go(startingNode, startingId)
    (goal, id - startingId)
  }

  def lcm(nums: Seq[Long]): Long = {
    @tailrec
    def _gcd(a: Long, b: Long): Long = if (a == b) a else _gcd(Math.max(a, b) - Math.min(a, b), Math.min(a, b))
    def _lcm(a: Long, b: Long): Long = if (a != 0 || b != 0) a * b / _gcd(a, b) else 0
    nums.reduce(_lcm)
  }

  def task1(dataLines: Seq[String]): Int = {
    val instructions = parseInstructions(dataLines.head)
    val nodes = parseNodes(dataLines.drop(2))
    val (_, steps) = getToGoal(instructions, nodes, "AAA", _ == "ZZZ")
    steps
  }

  def task2(dataLines: Seq[String]): Long = {
    val instructions = parseInstructions(dataLines.head)
    val nodes = parseNodes(dataLines.drop(2))
    val startingPoints = nodes.keys.filter(_.endsWith("A"))
    // Brute force (executing the steps in a loop until we are in a goal state)
    // will time out here, smarter solution needed
    val goalsWithOffsets = startingPoints.map(sp => getToGoal(
      instructions, nodes, sp, _.endsWith("Z")
    )).toMap
    val loopSizeForGoals = goalsWithOffsets.map { case (goal, offset) =>
      getToGoal(instructions, nodes, goal, _ == goal, offset, _ > offset)
    }
    // At this point we already know
    // - we can reach a unique goal node from each starting point
    // - continuing from the goal node, they loop back to themselves
    // - when stopping with the debugger, we can see that
    goalsWithOffsets.forall { case (goal, offset) => loopSizeForGoals(goal) == offset }
    // Because of this, the solutions is a simple least common multiplier, and not something
    // more complex, such as the generalisation of the algo below for n "walkers"
    // https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset
    lcm(loopSizeForGoals.values.map(_.toLong).toSeq)
  }
}
