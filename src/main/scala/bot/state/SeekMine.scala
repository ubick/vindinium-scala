package bot.state

import bot._
import bot.pathfind.{Path, Pathfinder}

class SeekMine(implicit val hero: Hero, implicit val board: PositionedBoard, implicit val pathfinder: Pathfinder) extends Behavior {
  override def run(): Path = pathToClosestMine

  override def validate(): ValidReason = {
    val lifeToWinMine: Int = pathToClosestMine.length + 21

    if (hero.life > lifeToWinMine) ValidReason(valid = true, s"Closest mine requires $lifeToWinMine HP, while hero as ${hero.life} HP.")
    else ValidReason(valid = false)
  }

  override def toString: String = "Seek Mine"

  private val pathToClosestMine: Path = {
    val mines: Vector[PositionedTile] = board.positionedTiles collect {
      case PositionedTile(Tile.Mine(owner), pos) if owner.isEmpty || owner.exists( hero.id !=) => board at pos get
    }
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)


    pathfinder.multiGoalFind(mines, heroTile)
  }
}