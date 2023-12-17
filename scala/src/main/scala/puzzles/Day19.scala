package puzzles

object Day19 {
  type Workflows = Map[String, Seq[String]]
  case class Part(x: Int, m: Int, a: Int, s: Int) {
    def get(field: String): Int = field match {
      case "x" => x
      case "m" => m
      case "a" => a
      case "s" => s
    }
    def sum: Int = x + m + a + s
  }

  def parse(in: Seq[String]): (Workflows, Seq[Part]) = {
    val (rawWorkflows, rawParts) = in.splitAt(in.indexWhere(_.isEmpty))
    val workflows = rawWorkflows.map(rwf => {
      val Array(name, rest) = rwf.dropRight(1).split("\\{")
      name -> rest.split(",").toSeq
    }).toMap
    val parts = rawParts.drop(1).map(rp => {
      val catMap = rp.drop(1).dropRight(1).split(",").map(cat => {
        val asd = cat.split("=")
        asd.head -> asd.last.toInt
      }).toMap
      Part(catMap("x"), catMap("m"), catMap("a"), catMap("s"))
    })
    (workflows, parts)
  }

  def isAccepted(part: Part, workflows: Workflows): Boolean = {
    @scala.annotation.tailrec
    def go(name: String, ruleId: Int): Boolean = {
      workflows(name)(ruleId) match {
        case s"$rule:$tgt" =>
          val cat = rule.head
          val cmp = rule(1) match {
            case '<' => (a: Int, b: Int) => a < b
            case '>' => (a: Int, b: Int) => a > b
          }
          val amt = rule.drop(2).toInt
          val partAmt = part.get(cat.toString)
          if (cmp(partAmt, amt)) {
            tgt match {
              case "A" => true
              case "R" => false
              case other => go(other, 0)
            }
          } else {
            go(name, ruleId + 1)
          }
        case "A" => true
        case "R" => false
        case tgt => go(tgt, 0)
      }
    }
    go("in", 0)
  }

  trait Node {
    val rule: String
  }
  case class Leaf(rule: String) extends Node
  case class Branch(rule: String, t: Node, f: Node) extends Node
  def buildGraph(workflows: Workflows): Node = {
    def go(name: String, ruleId: Int): Node = {
      workflows(name)(ruleId) match {
        case s"$rule:$tgt" =>
          val t = tgt match {
            case "A" => Leaf("A")
            case "R" => Leaf("R")
            case other => go(other, 0)
          }
          Branch(rule, t, f = go(name, ruleId + 1))
        case "A" => Leaf("A")
        case "R" => Leaf("R")
        case tgt => go(tgt, 0)
      }
    }
    go("in", 0)
  }

  case class Range(lower: Int, upper: Int)
  case class EvaluationSummary (result: String, m: Map[String, Range])

  def evaluationSummaries(graph: Node, ranges: Map[String, Range]): Seq[EvaluationSummary] = {
    graph match {
      case Branch(rule, t, f) =>
        val (tRanges, fRanges) = rule match {
          case s"$cat>$amt" =>
            val currentRange = ranges(cat)
            val tr = ranges.updated(cat, currentRange.copy(lower = amt.toInt + 1))
            val fr = ranges.updated(cat, currentRange.copy(upper = amt.toInt))
            (tr, fr)
          case s"$cat<$amt" =>
            val currentRange = ranges(cat)
            val tr = ranges.updated(cat, currentRange.copy(upper = amt.toInt - 1))
            val fr = ranges.updated(cat, currentRange.copy(lower = amt.toInt))
            (tr, fr)
        }
        val tPaths = evaluationSummaries(t, tRanges)
        val fPaths = evaluationSummaries(f, fRanges)
        tPaths ++ fPaths
      case Leaf(rule) => Seq(EvaluationSummary(rule, ranges))
    }
  }

  def task1(in: Seq[String]): Int = {
    val (workflows, parts) = parse(in)
    parts.filter(p => isAccepted(p, workflows)).map(_.sum).sum
  }

  def task2(in: Seq[String]): Long = {
    val (workflows, parts) = parse(in)
    val g = buildGraph(workflows)
    val es = evaluationSummaries(g, Map(
      "x" -> Range(1, 4000),
      "m" -> Range(1, 4000),
      "a" -> Range(1, 4000),
      "s" -> Range(1, 4000)
    ))
    val acceptedRanges = es.filter(_.result == "A")
    acceptedRanges.map(_.m.values.map(r => r.upper - r.lower + 1).map(_.toLong).product).sum
  }

}
