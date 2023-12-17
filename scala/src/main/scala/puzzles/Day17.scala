package puzzles

object Day17 {
  var MIN_STRAIGHT_STEPS = 0
  var MAX_STRAIGHT_STEPS = 3

  case class Blocks(blocks: Seq[Seq[Int]]) {
    def h: Int = blocks.size
    def w: Int = blocks.head.size
    def get(pos: Pos): Int = blocks(pos.x)(pos.y)
  }

  case class Pos(x: Int, y: Int) {
    def isValid(blocks: Blocks): Boolean = x >= 0 && y >= 0 && x < blocks.h && y < blocks.w
    def stepUp: Pos = copy(x = x - 1)
    def stepDown: Pos = copy(x = x + 1)
    def stepLeft: Pos = copy(y = y - 1)
    def stepRight: Pos = copy(y = y + 1)
    def step(dir: String): Pos = dir match {
      case "U" => stepUp
      case "D" => stepDown
      case "L" => stepLeft
      case "R" => stepRight
    }
  }

  case class Step(pos: Pos, dir: String, stepsInDirection: Int) {
    val canStep = Map(
      "S" -> Seq("U", "D", "L", "R"),
      "U" -> Seq("U", "L", "R"),
      "D" -> Seq("D", "L", "R"),
      "L" -> Seq("L", "U", "D"),
      "R" -> Seq("R", "U", "D")
    )

    def waysOut(blocks: Blocks): Seq[Step] = {
      val turningSteps = if (dir == "S" || stepsInDirection >= MIN_STRAIGHT_STEPS){
        canStep(dir).filterNot(_ == dir).map(o => Step(pos.step(o), o, stepsInDirection = 1))
      } else {
        Seq.empty
      }
      val straightSteps = if (dir != "S" && stepsInDirection < MAX_STRAIGHT_STEPS)
        Seq(Step(pos.step(dir), dir, stepsInDirection + 1))
      else {
        Seq.empty
      }
      turningSteps.appendedAll(straightSteps)
    }.filter(_.pos.isValid(blocks))

    def value(blocks: Blocks): Int = blocks.get(pos)
  }

  def parse(in: Seq[String]): Blocks = Blocks(in.map(_.toCharArray.map(_.toString.toInt)))

  def astar(blocks: Blocks, start: Pos, dest: Pos): Option[Seq[Step]] = {
    def reconstructPath(cameFrom: Map[Step, Step], current: Step): Seq[Step] = {
      val totalPath = collection.mutable.ListBuffer(current)
      var c = current
      while (cameFrom.contains(c)) {
        c = cameFrom(c)
        totalPath.prepend(c)
      }
      totalPath.toSeq

    }
    def h(node: Step): Int = dest.x - node.pos.x + dest.y - node.pos.y
    val startStep = Step(start, "S", stepsInDirection = 0)
    val openSet = collection.mutable.PriorityQueue.empty[(Step, Int)](Ordering.by[(Step, Int), Int](_._2).reverse)
    val seen = collection.mutable.HashSet.empty[Step]
    val cameFrom = collection.mutable.HashMap.empty[Step, Step]
    val gScore = collection.mutable.HashMap[Step, Int](startStep -> 0)

    openSet.enqueue((startStep, h(startStep)))
    seen.addOne(startStep)

    while (openSet.nonEmpty) {
      val (current, fScore) = openSet.dequeue()
      if (current.pos == dest) {
        return Some(reconstructPath(cameFrom.toMap, current))
      }
      current.waysOut(blocks).foreach(neighbor => {
        val d = neighbor.value(blocks)
        val tentativeGScore = gScore(current) + d
        if (gScore.get(neighbor).forall(_ > tentativeGScore)) {
          cameFrom.update(neighbor, current)
          gScore.update(neighbor, tentativeGScore)
          if (!seen.contains(neighbor)) {
            seen.addOne(neighbor)
            openSet.enqueue((neighbor, tentativeGScore + h(neighbor)))
          }
        }
      })
    }
    None
  }

  def task(in: Seq[String]): Int = {
    val blocks = parse(in)
    val path = astar(blocks, Pos(0, 0), Pos(blocks.h - 1, blocks.w - 1)).get
    path.tail.map(_.value(blocks)).sum
  }

  def task1(in: Seq[String]): Int = {
    MIN_STRAIGHT_STEPS = 0
    MAX_STRAIGHT_STEPS = 3
    task(in)
  }

  def task2(in: Seq[String]): Int = {
    MIN_STRAIGHT_STEPS = 4
    MAX_STRAIGHT_STEPS = 10
    task(in)
  }
}
