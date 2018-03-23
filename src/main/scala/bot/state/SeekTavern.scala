package bot.state

import bot.GameEngine.GameContext
import bot._
import bot.pathfind.{Path, Pathfinder}

class SeekTavern(implicit val context: GameContext) extends Behavior {
  val hero: Hero = context.hero
  val board: PositionedBoard = context.board
  val pathfinder: Pathfinder = context.pathfinder

  val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)

  override def run(): Path = pathfinder.multiGoalFind(board.taverns, heroTile)

  override def validate(): ValidReason = {
    val path: Path = pathfinder.multiGoalFind(board.taverns, heroTile)
    val lifeToWinMine: Int = pathToClosestMine.length + 21

    if (hero.gold < 2) ValidReason(valid = false)
    else if (path.length == 1 && hero.life < 91) ValidReason(valid = true, "tavern path == 1 and hero life < 91")
    else if (hero.life < lifeToWinMine) ValidReason(valid = true, "Not enough life to travel to closest mine")
    else if (hero.life < 21) ValidReason(valid = true, "hero life < 21")
    else if (hero.mineCount > board.mines.length / 2) ValidReason(valid = true, "Owning 50% of all mines. Camping at the pub.")
    else ValidReason(valid = false)
  }

  override def toString: String = "Seek Tavern"

  private val pathToClosestMine: Path = {
    val mines: Vector[PositionedTile] = board.positionedTiles collect {
      case PositionedTile(Tile.Mine(owner), pos, _) if owner.isEmpty || owner.exists( hero.id !=) => board at pos get
    }
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)

    pathfinder.multiGoalFind(mines, heroTile)
  }

  private val mines: Vector[PositionedTile] = board.positionedTiles collect {
    case PositionedTile(Tile.Mine(owner), pos, _) if owner.isEmpty || owner.exists( hero.id !=) => board at pos get
  }
}
