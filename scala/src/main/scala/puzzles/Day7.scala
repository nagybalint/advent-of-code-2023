package puzzles

trait PlayingCard {
  val label: String
  def jValue: Int
  def value: Int = label match {
    case "A" => 14
    case "K" => 13
    case "Q" => 12
    case "J" => jValue
    case "T" => 10
    case n if n.toIntOption.isDefined => n.toInt
  }
}

case class SimpleCard(label: String) extends PlayingCard {
  override def jValue: Int = 11
}

case class JokerCard(label: String) extends PlayingCard {
  override def jValue: Int = 1
  def isJoker: Boolean = label == "J"
}

trait Hand[C <: PlayingCard] extends Ordered[Hand[C]]{
  val cards: Seq[C]
  val bid: Int
  def handType: Seq[Int]
  override def compare(that: Hand[C]): Int =
    this.handType.zip(that.handType).dropWhile { case (a, b) => a == b }.headOption match {
      case Some((myType, otherType)) => if (myType < otherType) -1 else 1
      case None => this.cards.zip(that.cards).dropWhile { case (a, b) => a == b }.headOption match {
        case Some((myCard, otherCard)) => if (myCard.value < otherCard.value) -1 else 1
        case None => 0
      }
    }
}

case class SimpleHand(cards: Seq[SimpleCard], bid: Int) extends Hand[SimpleCard] {
  override def handType: Seq[Int] =
    cards.groupBy(_.label).map { case (_, cards) => cards.size }.toSeq.sorted.reverse
}

case class JokerHand(cards: Seq[JokerCard], bid: Int) extends Hand[JokerCard] {
  override def handType: Seq[Int] =
    cards.filterNot(_.isJoker).groupBy(_.label).map { case (_, cards) => cards.size }.toSeq.sorted.reverse match {
      case s if s.isEmpty => Seq(5)
      case s => s.updated(0, s.head + cards.count(_.isJoker))
    }
}

object Day7 {
  private def parseSimpleHands(dataLines: Seq[String]): Seq[SimpleHand] =
    dataLines.map(hand => {
      val x = hand.split(" ")
      val cards = x.head.toCharArray.map(_.toString).map(SimpleCard)
      val bid = x.last.toInt
      SimpleHand(cards, bid)
    })

  private def parseJokerHands(dataLines: Seq[String]): Seq[JokerHand] =
    dataLines.map(hand => {
      val x = hand.split(" ")
      val cards = x.head.toCharArray.map(_.toString).map(JokerCard)
      val bid = x.last.toInt
      JokerHand(cards, bid)
    })

  def task[C <: PlayingCard](hands: Seq[Hand[C]]): Int =
    hands.sorted.zipWithIndex.map { case (hand, i) => hand.bid * (i + 1) }.sum

  def task1(dataLines: Seq[String]): Long = task(parseSimpleHands(dataLines))
  def task2(dataLines: Seq[String]): Long = task(parseJokerHands(dataLines))
}
