package puzzles

object Day4 {

  case class Card(id: Int, winningNumbers: Seq[Int], ownNumbers: Seq[Int]) {
    def hits: Seq[Int] = winningNumbers.intersect(ownNumbers)
    def points: Int = Math.pow(2, this.hits.size - 1).toInt
  }

  private def parseCards(rawCards: Seq[String]): Seq[Card] =
    rawCards.map {
      case s"Card $id: $winning | $own" => Card(
        id = id.strip().toInt,
        winningNumbers = winning.split(" ").filterNot(_.isBlank).map(_.toInt),
        ownNumbers = own.split(" ").filterNot(_.isBlank).map(_.toInt)
      )
    }

  def task1(rawCards: Seq[String]): Int = parseCards(rawCards).map(_.points).sum

  def task2(rawCards: Seq[String]): Int = {
    def addCardsWon(cardBalance: Map[Int, Int], winningCard: Card): Map[Int, Int] =
      (winningCard.id + 1 to winningCard.id + winningCard.hits.size).foldLeft(cardBalance) {
        case (cardBalance, cardWon) =>
          val currentBalance = cardBalance(cardWon)
          val winnings = cardBalance(winningCard.id)
          cardBalance.updated(cardWon, currentBalance + winnings)
      }

    def getOneOfEach(cards: Seq[Card]): Map[Int, Int] = Map.from(cards.map(c => (c.id, 1)))

    val cards = parseCards(rawCards)
    cards.foldLeft(getOneOfEach(cards)) {
      case (cardBalance, card) => addCardsWon(cardBalance, card)
    }.values.sum
  }
}
