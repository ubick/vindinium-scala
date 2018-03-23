package bot

import bot.Dir.Stay
import bot.pathfind.Pathfinder
import bot.state._

object GameEngine {

  def play(input: Input, skipBoard: Boolean = false): DirReason = {
    val hero: Hero = input.hero
    val simpleBoard: PositionedBoard = PositionedBoard(input.game.board.size, PositionedBoard.positionedTiles(input.game.board))

    val board: PositionedBoard = simpleBoard.copy(positionedTiles =
      simpleBoard.weightedTiles(input.game.heroes.toVector.filterNot(_.id == hero.id), input.hero))

    val enemyHeroes: Vector[Hero] = input.game.heroes.toVector filter (_.id != input.hero.id)
    implicit val context: GameContext = GameContext(board, input.hero, enemyHeroes, new Pathfinder(input, board))

    val behaviors: List[Behavior] = List(
      new SeekHero(),
      new ChaseRichHero(),
      new SeekTavern(),
      new FleeHero(),
      new SeekMine()
    )

    val dirReason: DirReason = behaviors collectFirst {
      case b if b.validate().valid => {
        val path = b.run()
        val pathDebug: String = s"${path.nextDir.toString.padTo(5, " ").mkString} | ${path.next} => ${path.destination}".replace("Some(", "").replace(")", "")
        val fullReason = s"$pathDebug - $b: ${b.validate().reason}"

        DirReason(path.nextDir, fullReason, Some(path))
      }
    } getOrElse DirReason(Stay, "No behavior matched.")

    dirReason.copy(reason = Renderer.renderBoard(board, input, dirReason, skipBoard))
  }

  case class GameContext(board: PositionedBoard, hero: Hero, enemyHeroes: Vector[Hero], pathfinder: Pathfinder)
}
