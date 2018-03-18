package bot.state

import bot.GameEngine.GameContext
import bot._
import bot.pathfind.{Path, Pathfinder}

class SeekMine(implicit val context: GameContext) extends Behavior {
  val hero: Hero = context.hero
  val board: PositionedBoard = context.board
  val pathfinder: Pathfinder = context.pathfinder

  override def run(): Path = pathToClosestMine

  override def validate(): ValidReason = {
    val lifeToWinMine: Int = pathToClosestMine.length + 21
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)
    val closestTavernPath: Path = pathfinder.multiGoalFind(board.taverns, heroTile)

    if (pathToClosestMine.isEmpty || closestTavernPath.length == 1 && hero.life < 91) ValidReason(valid = false)
    else if (hero.life > lifeToWinMine) ValidReason(valid = true, s"Closest mine requires $lifeToWinMine HP, while hero as ${hero.life} HP.")
    else ValidReason(valid = false)
  }

  override def toString: String = "Seek Mine"

  private val pathToClosestMine: Path = {
    val mines: Vector[PositionedTile] = board.positionedTiles collect {
      case PositionedTile(Tile.Mine(owner), pos, _) if owner.isEmpty || owner.exists( hero.id !=) => board at pos get
    }
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)

    pathfinder.multiGoalFind(mines, heroTile)
  }
}