package bot

import bot.Tile.{Air, Mine, Tavern, Wall}

object Renderer {
  val YELLOW = "\u001B[33m"
  val CYAN = "\u001B[36m"
  val GREEN = "\u001B[32m"
  val RED = "\u001B[31m"
  val RESET = "\u001B[0m"

  def renderBoard(board: PositionedBoard, input: Input, dirReason: DirReason, skipBoard: Boolean = false): String = {
    def printPositionedTile(pt: PositionedTile): String = pt.tile match {
      case Wall => s"▓▓▓▓▓"
      case Air if dirReason.path.exists(_.positionedTiles.exists(_.pos == pt.pos)) => s"$YELLOW  " + s"${pt.weight}▶".padTo(3, " ").mkString + RESET
      case Air => s"  ${pt.weight}"
      case Mine(Some(id)) if id == input.hero.id => s"$GREEN  ◧ $id$RESET"
      case Mine(Some(id)) => s"$CYAN  ◧ $id$RESET"
      case Mine(None) => s"$CYAN  ◧  $RESET"
      case Tavern => s"$YELLOW PUB $RESET"
      case Tile.Hero(id) if id == input.hero.id => s"$GREEN  😀  $RESET"
      case Tile.Hero(id) => s"$RED  😈 $id$RESET"
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

    val outputBeforeEnd: String = s"Life: ${input.hero.life} | ${dirReason.reason}" + (if (skipBoard) "" else s"\n\n$renderedBoard")

    if (skipBoard) outputBeforeEnd
    else if (input.game.turn == input.game.maxTurns - 3) {
      outputBeforeEnd + "\33[1A" * (board.size + 3)
    } else outputBeforeEnd
  }
}
