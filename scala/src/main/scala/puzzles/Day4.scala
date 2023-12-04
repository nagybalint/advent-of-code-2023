package puzzles
case class Card(id: Int, winningNumbers: Seq[Int], ownNumbers: Seq[Int]) {
  def hits = winningNumbers.intersect(ownNumbers)
  def points = Math.pow(2, this.hits.size - 1).toInt
}
object Day4 {

  private def parseCards(cards: Seq[String]): Seq[Card] = {
    cards.map {
      case s"Card $id: $winning | $own" => Card(
        id = id.strip().toInt,
        winningNumbers = winning.split(" ").filterNot(_.isBlank).map(_.toInt),
        ownNumbers = own.split(" ").filterNot(_.isBlank).map(_.toInt)
      )
    }

  }
  def task1(cards: Seq[String]): Int = parseCards(cards).map(_.points).sum

  def task2(cards: Seq[String]): Int = {
    def addCardsWon(currentCards: Map[Int, Int], card: Card): Map[Int, Int] =
      (card.id + 1 to card.id + card.hits.size).foldLeft(currentCards) {
        case (currentCards, id) => currentCards.updated(id, currentCards(id) + currentCards(card.id))
      }

    def getStartingCards(cards: Seq[Card]): Map[Int, Int] =
      cards.foldLeft(Map.empty[Int, Int]) { case (m, card) => m.updated(card.id, 1) }

    val parsedCards = parseCards(cards)
    parsedCards.foldLeft(getStartingCards(parsedCards)) {
      case (m, card) => addCardsWon(m, card)
    }.values.sum
  }
}
