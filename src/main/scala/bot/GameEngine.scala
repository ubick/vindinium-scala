package bot

import bot.Dir.Stay
import bot.pathfind.Pathfinder
import bot.state.{Behavior,  SeekHero, SeekTavern}

object GameEngine {
  def play(gameInput: Input): DirReason = {
    implicit val input: Input = gameInput
    implicit val board: PositionedBoard = PositionedBoard(input.game.board.size, PositionedBoard.positionedTiles(input.game.board))
    implicit val pathfinder: Pathfinder = new Pathfinder(input, board)

    val behaviors: List[Behavior] = List(
//      new SeekTavern(),
      new SeekHero()
    )

    behaviors collectFirst {
      case b if b.validate().valid => {
        val path = b.run()
        val pathDebug: String = s"${path.nextDir} | ${path.next} => ${path.destination}".replace("Some(", "").replace(")", "")
        val fullReason = s"$pathDebug - $b: ${b.validate().reason}"

        DirReason(path.nextDir, fullReason)
      }
    } getOrElse DirReason(Stay, "No behavior matched.")
  }
}
