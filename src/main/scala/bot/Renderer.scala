package bot

import bot.Tile.{Air, Mine, Tavern, Wall}

object Renderer {
  def renderBoard(board: PositionedBoard, input: Input, reason: String): String = {
    def printPositionedTile(pt: PositionedTile): String = pt.tile match {
      case Wall => s"▓▓▓▓▓"
      case Air => s"  ${pt.weight}"
      case Mine(Some(id)) if id == input.hero.id => s"\u001B[32m  ◧ $id" + "\u001B[0m"
      case Mine(Some(id)) => s"\u001B[36m  ◧ $id" + "\u001B[0m"
      case Mine(None) => s"\u001B[36m  ◧  " + "\u001B[0m"
      case Tavern => s" \u001B[33m🍻 🍻 [0m"
      case Tile.Hero(id) if id == input.hero.id => s"  \u001B[32m😀  \u001B[0m"
      case Tile.Hero(id) => s"  \u001B[31m😈 ${pt.weight}" + "\u001B[0m"
      case _ => s"?${pt.weight}"
    }

    val renderedBoard: String =
      board.positionedTiles.foldRight("") { (pt: PositionedTile, s: String) =>
        pt match {
          case PositionedTile(tile, pos, weight) =>
            val tileString = s + s"${printPositionedTile(pt).padTo(5, " ").mkString}"

            if (pos.y == board.size - 1) tileString + "\n"
            else tileString
          case _ => s + " ?? "
        }
      }

    val outputBeforeEnd: String = s"Life: ${input.hero.life} | $reason\n\n$renderedBoard"

    if (input.game.turn == input.game.maxTurns - 3) {
      outputBeforeEnd + "\33[1A" * (board.size + 3)
    } else outputBeforeEnd
  }
}
