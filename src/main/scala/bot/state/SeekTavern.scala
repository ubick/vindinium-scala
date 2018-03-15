package bot.state

import bot._
import bot.pathfind.{Path, Pathfinder}

class SeekTavern(implicit val hero: Hero, implicit val board: PositionedBoard, implicit val pathfinder: Pathfinder) extends Behavior {
  val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)

  override def run(): Path = pathfinder.multiGoalFind(board.taverns, heroTile)

  override def validate(): ValidReason = {
    val path: Path = pathfinder.multiGoalFind(board.taverns, heroTile)
    val lifeToWinMine: Int = pathToClosestMine.length + 21

    if (path.length == 1 && hero.life < 91) ValidReason(valid = true, "tavern path == 1 and hero life < 91")
    else if (hero.life < lifeToWinMine) ValidReason(valid = true, "Not enough life to travel to closest mine")
    else if (hero.life < 21) ValidReason(valid = true, "hero life < 21")
    else if (mines.isEmpty) ValidReason(valid = true, "No mines left to conquer")
    else ValidReason(valid = false)
  }

  override def toString: String = "Seek Tavern"

  private val pathToClosestMine: Path = {
    val mines: Vector[PositionedTile] = board.positionedTiles collect {
      case PositionedTile(Tile.Mine(owner), pos) if owner.isEmpty || owner.exists( hero.id !=) => board at pos get
    }
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)


    pathfinder.multiGoalFind(mines, heroTile)
  }

  private val mines: Vector[PositionedTile] = board.positionedTiles collect {
    case PositionedTile(Tile.Mine(owner), pos) if owner.isEmpty || owner.exists( hero.id !=) => board at pos get
  }
}
