package puzzles

object Day2 {

  private def parseGame(game: String): Seq[Seq[(String, Int)]] =
    game match {
      case s"Game $id: $rounds" => rounds.split("; ").map(cubes => {
        cubes.split(", ").map {
          case s"$n $color" => (color, n.toInt)
        }.toSeq
      }).toSeq
    }

  private def isCubeUnderLimit(cube: (String, Int)): Boolean =
    cube match {
      case ("red", r: Int) => r <= 12
      case ("green", g: Int) => g <= 13
      case ("blue", b: Int) => b <= 14
    }

  private def isGameValid(game: Seq[Seq[(String, Int)]]): Boolean =
    game.forall(round => round.forall(isCubeUnderLimit))

  private def getMinCubesForGame(game: Seq[Seq[(String, Int)]]): Map[String, Int] =
    game.foldLeft(
      Map("red" -> 0, "green" -> 0, "blue" -> 0)
    )(
      (maxCubes, round) => round.foldLeft(maxCubes) {
        case (maxCubes, (color, n)) => maxCubes.updated(color, math.max(maxCubes(color), n))
      }
    )

  private def getGamePower(game: Seq[Seq[(String, Int)]]): Int = getMinCubesForGame(game).values.product

  def task1(games: Seq[String]): Int =
    games
      .map(parseGame)
      .zipWithIndex
      .filter { case (game, _) => isGameValid(game) }
      .map { case (_, id) => id + 1 }
      .sum

  def task2(games: Seq[String]): Int = games.map(parseGame).map(getGamePower).sum
}
