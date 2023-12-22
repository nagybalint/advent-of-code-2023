package puzzles

object Day20 {
  type Level = Boolean
  val IGNORE_LEVEL = false

  trait Module {
    val name: String
    val outputs: Seq[String]
    val state: Level
    def updateState(pulse: Pulse): Module
    def sendPulses(level: Level): Seq[Pulse]
  }

  case class Button() extends Module {
    override val name: String = "btn"
    override val outputs: Seq[String] = Seq("broadcaster")
    override val state: Level = false
    override def updateState(pulse: Pulse): Module = this
    override def sendPulses(level: Level): Seq[Pulse] = Seq(Pulse(level = false, name, "broadcaster"))
  }

  case class Broadcaster(outputs: Seq[String], state: Level = false) extends Module {
    override val name: String = "broadcaster"
    override def updateState(pulse: Pulse): Module = copy(state = pulse.level)
    override def sendPulses(level: Level): Seq[Pulse] = outputs.map(Pulse(state, name, _))
  }

  case class FlipFlop(name: String, outputs: Seq[String], state: Level = false) extends Module {
    override def updateState(pulse: Pulse): Module = if (pulse.level) this else copy(state = !state)
    override def sendPulses(level: Level): Seq[Pulse] = if (level) Seq.empty else outputs.map(Pulse(state, name, _))
  }

  case class Conjunction(name: String, outputs: Seq[String], inputStates: Map[String, Level] = Map.empty) extends Module {
    override val state: Level = if (inputStates.values.forall(_ == true)) false else true
    override def updateState(pulse: Pulse): Module = copy(inputStates = inputStates.updated(pulse.source, pulse.level))
    override def sendPulses(level: Level): Seq[Pulse] =
      if (inputStates.values.forall(_ == true)) outputs.map(Pulse(false, name, _)) else outputs.map(Pulse(true, name, _))
  }

  case class Output(name: String, state: Boolean = false) extends Module {
    override val outputs: Seq[String] = Seq.empty
    override def updateState(pulse: Pulse): Module = copy(state = pulse.level)
    override def sendPulses(level: Level): Seq[Pulse] = Seq.empty
  }

  object Modules {
    def parseOutputs(str: String): Array[String] = str.split(", ")
    def from(modules: Seq[String]): Seq[Module] = {
      val mods: Seq[Module] = modules.map {
        case s"broadcaster -> $outputs" => Broadcaster(parseOutputs(outputs))
        case s"%$name -> $outputs" => FlipFlop(name, parseOutputs(outputs))
        case s"&$name -> $outputs" => Conjunction(name, parseOutputs(outputs))
        case _ => throw new IllegalStateException("sad")
      }
      mods.map {
        case Conjunction(name, outputs, _) =>
          val inputStates = mods.filter(_.outputs.contains(name)).map(_.name).map(n => n -> false).toMap
          Conjunction(name, outputs, inputStates)
        case x => x
      }.appendedAll(mods.flatMap(_.outputs).toSet.removedAll(mods.map(_.name).toSet).map(Output(_)))
    }

  }

  case class Pulse(level: Level, source: String, destination: String)

  def execute(ms: Map[String, Module], buttonPresses: Int): Long = {
    val modules = collection.mutable.HashMap.from(ms)
    val q = collection.mutable.Queue.empty[Pulse]
    val memo = collection.mutable.ListBuffer.empty[Pulse]
    val button: Module = Button()
    (0 until buttonPresses).foreach(_ => {
      val initialPulse = button.sendPulses(IGNORE_LEVEL)
      q.addAll(initialPulse)
      while (q.nonEmpty) {
        val p = q.dequeue()
        memo.addOne(p)
        modules.update(p.destination, modules(p.destination).updateState(p))
        q.enqueueAll(modules(p.destination).sendPulses(p.level))
      }
    }
    )
    memo.toList.groupBy(_.level).values.map(_.size.toLong).product
  }

  def findCycle(ms: Map[String, Module], broadcasterChild: String, rxParent: String): Long = {
    val modules = collection.mutable.HashMap.from(ms)
    val rxPa = modules(rxParent).asInstanceOf[Conjunction]
    modules.update("broadcaster", modules("broadcaster").asInstanceOf[Broadcaster].copy(outputs = Seq(broadcasterChild)))
    modules.update(rxParent, rxPa.copy(inputStates = rxPa.inputStates.map(p => (p._1, true))))
    val q = collection.mutable.Queue.empty[Pulse]
    val button: Module = Button()
    var i = 0
    while (true) {
      i += 1
      val initialPulse = button.sendPulses(IGNORE_LEVEL)
      q.addAll(initialPulse)
      while (q.nonEmpty) {
        val p = q.dequeue()
        if (p.destination == "rx" && !p.level) {
          return i
        }
        modules.update(p.destination, modules(p.destination).updateState(p))
        q.enqueueAll(modules(p.destination).sendPulses(p.level))
      }
    }
    throw new Exception("Unreachable code")
  }

  def task1(in: Seq[String]): Long = execute(Modules.from(in).map(m => m.name -> m).toMap, 1000)

  def lcm(nums: Seq[Long]): Long = {
    @scala.annotation.tailrec
    def _gcd(a: Long, b: Long): Long = if (a == b) a else _gcd(Math.max(a, b) - Math.min(a, b), Math.min(a, b))
    def _lcm(a: Long, b: Long): Long = if (a != 0 || b != 0) a * b / _gcd(a, b) else 0
    nums.reduce(_lcm)
  }

  def task2(in: Seq[String]): Long = {
    val modules = Modules.from(in).map(m => m.name -> m).toMap
    val rxParent = modules.find(_._2.outputs.contains("rx")).get._1
    lcm(modules("broadcaster").outputs.map(bcChild => findCycle(modules, bcChild, rxParent)))
  }
}
